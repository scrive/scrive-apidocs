/** @jsx React.DOM */

define(['lists/paginationmodel','lists/sortingmodel','lists/textfilteringmodel','lists/selectfilteringmodel','legacy_code'], function(PagingModel,SortingModel,TextFilteringModel,SelectFilteringModel) {

    var ListObject = Backbone.Model.extend({
        defaults: function() {
          return {
            selected: false,
            expanded: false,
            listObjectData : {}
          };
        },
        initialize: function (args) {
            if (this.collection != undefined && this.collection.namespace() != undefined &&  this.id() != undefined)
            {
                var namespace =this.collection.namespace();
                var val = SessionStorage.get(namespace, "expanded" + this.id());
                if (val != undefined && val != "")
                  this.set({ "expanded": val == "true" });
            }
        },
        id : function() {
          if (this.collection.idFetcher)
            return this.collection.idFetcher()(this);
        },
        field: function(name) {
            return this.get("listObjectData")[name];
        },
        setProperty : function(name,value) {
         var obj = {};
         obj[name] = value;
         this.set(obj);
        },
        getProperty : function(name) {
          return this.get(name);
        },
        isSelected: function() {
            return this.get("selected") == true;
        },
        select: function() {
            this.set({ "selected": true });
        },
        unselect: function() {
            this.set({ "selected": false });
        },
        toogleSelect : function() {
            this.set({ "selected": !this.isSelected()});
        },
        isExpanded: function() {
            return this.get("expanded") == true;
        },
        toggleExpand: function() {
            var val = this.isExpanded();
            var namespace = this.collection.namespace();
            var id = this.id();
            SessionStorage.set(namespace, "expanded" + id, "" + !val);
            this.set({ "expanded": !val });
            return false;
        }
    });

    var ListOfObjects = Backbone.Collection.extend({
        model: function(attrs,options) {
            return new ListObject({listObjectData : attrs},options);

        },
        initialize: function(models,args) {
          this._namespace = args.namespace;
          this._idFetcher = args.idFetcher;
        },
        namespace : function() {
          return this._namespace;
        },
        idFetcher : function() {
          return this._idFetcher;
        },
        getSelected: function() {
            return this.filter(function(e) { return e.isSelected(); });
        },
        isEmpty: function() {
            return this.models.length === 0;
        },
        selectNone: function() {
            this.each(function(e) {
                e.unselect();
            });
        },
        selectAll: function() {
            this.each(function(e) {
                e.select();
            });
        },
        toogleSelect : function() {
          if (this.any(function(e) {return !e.isSelected()}))
            this.selectAll();
          else
            this.selectNone();
        }
    });


return Backbone.Model.extend({
    defaults: function() {
      return {
            list : undefined,
            url : undefined,
            ready : false,
            dataFetcher : function(d) {return d;},
            idFetcher : function(d) {return d.field("id");},
            onReload : function() {},
            namespace : Math.random() + "",
            paging: new PagingModel({}),
            sorting: new SortingModel({}),
            textfiltering : new TextFilteringModel({}),
            selectfiltering : new SelectFilteringModel({})
      };
    },
    initialize : function(args) {
      var self = this;
      this.url = args.url;
      this.textfiltering().on("change", function() {self.reload()});
      this.sorting().on("change", function() {self.reload()});
      this.selectfiltering().on("change", function() {self.reload()});
      if (args.loadLater != true)
        this.reload();
    },
    urlParams : function() {
      var params = {};
      params.page = this.paging().pageCurrent();
      params.offset = params.page * this.paging().pageSize();
      params.sort = this.sorting().current();
      params.sortReversed = this.sorting().isAsc();
      params.textfilter = this.textfiltering().text();
      params.selectfilter = this.selectfiltering().asJSON();

      return params;
    },
    dataFetcher : function() {
      return this.get("dataFetcher");
    },
    idFetcher : function() {
      return this.get("idFetcher");
    },
    ready : function() {
      return this.get("ready");
    },
    list : function() {
      return this.get("list");
    },
    paging : function() {
      return this.get("paging");
    },
    namespace : function() {
      return this.get("namespace");
    },
    sorting : function() {
      return this.get("sorting");
    },
    textfiltering : function() {
      return this.get("textfiltering");
    },
    selectfiltering : function() {
      return this.get("selectfiltering");
    },
    checkIfChangedAndCallback : function(changedcallback,errorcallback) {
      var self = this;
      $.ajax({
        url : this.url,
        method: "GET",
        data : this.urlParams(),
        cache: false,
        success : function(res) {
          if (self.list() != undefined && self.dataFetcher()(JSON.parse(res)).length != self.list().length)
            changedcallback();
        },
        error : function() {
          if (errorcallback != undefined)
            errorcallback(); }
        });
    },
    reload : function(callback) {
      var self = this;
      this.set({ready : false});
      this.fetch({
        data: this.urlParams(),
        processData: true,
        cache: false
      }).done(function() {
        self.get("onReload")();
        if (callback) {
          callback();
        }
      });
    },
    parse : function(data) {
      var self = this;
      var list = new ListOfObjects(this.dataFetcher()(data), {namespace : this.namespace(),idFetcher : this.idFetcher()});
      var paging = new PagingModel(data.paging);
      list.on("change", function() {self.trigger("change")});
      paging.on("change", function() {self.reload()});
      paging.switchToEarlierPageIfThatsNeeded(); // If needed this has to be trigger after binding on change
      return {
        ready: true,
        list : list,
        paging : paging
      };
    }

});

});

