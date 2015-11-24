/** @jsx React.DOM */

define(['lists/sortingmodel','lists/textfilteringmodel','lists/selectfilteringmodel','legacy_code'], function(SortingModel,TextFilteringModel,SelectFilteringModel) {

    var ListObject = Backbone.Model.extend({
        defaults: function() {
          return {
            selected: false,
            expanded: false,
            listObjectData : {}
          };
        },
        initialize: function (args) {
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
            this.set({ "expanded": !this.isExpanded() });
            return false;
        }
    });

    var ListOfObjects = Backbone.Collection.extend({
        model: function(attrs,options) {
            return new ListObject({listObjectData : attrs},options);

        },
        initialize: function(models,args) {
          this._idFetcher = args.idFetcher;
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
      var self = this;
      return {
            list : undefined,
            url : undefined,
            ready : false,
            dataFetcher : function(d) {return d;},
            idFetcher : function(d) {return d.field("id");},
            onReload : function() {},
            sorting: new SortingModel({}),
            textfiltering : new TextFilteringModel({}),
            selectfiltering : new SelectFilteringModel({}),
            paramsFunction : function() { return {};},
            offset: 0,
            maxPageSize: 100,
            totalCountFunction: function(d) {return self.dataFetcher()(d),length},
            totalCount: 0
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
      return this.get("paramsFunction")( this.textfiltering().text(), this.selectfiltering(), this.sorting(), this.offset(), this.maxPageSize());
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
    sorting : function() {
      return this.get("sorting");
    },
    textfiltering : function() {
      return this.get("textfiltering");
    },
    selectfiltering : function() {
      return this.get("selectfiltering");
    },
    offset: function() {
      return this.get("offset");
    },
    changeOffsetAndReload: function(offset) {
      this.set({"offset" : offset}, {silent: true});
      this.reload();
    },
    maxPageSize: function() {
      return this.get("maxPageSize");
    },
    totalCount : function() {
      return this.get("totalCount");
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
      var list = new ListOfObjects(this.dataFetcher()(data), {idFetcher : this.idFetcher()});
      list.on("change", function() {self.trigger("change")});
      var totalCount = self.get("totalCountFunction")(data, this.offset());
      // After getting new data we may discover that we are on page that is empty
      if (totalCount <= this.offset() && this.offset() > 0) {
        var numberOfFullPages = Math.floor((totalCount -1) / this.maxPageSize());
        this.changeOffsetAndReload(numberOfFullPages * this.maxPageSize());
        return {};
      } else {
        return {
          ready: true,
          list : list,
          totalCount : totalCount
        };
      }
    }

});

});

