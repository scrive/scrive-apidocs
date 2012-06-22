/*
 * Schema of tables. Schema is a commection of cells that define what to show + Paging,Sorting and Filtering objects that affect what is downloaded from server.
 */


(function(window) {
    window.Cell = Backbone.Model.extend({
        name: function() {
            return this.get("name");
        },
        field: function() {
            return this.get("field");
        },
        subfield: function() {
            if (this.get("subfield") != undefined) {
                  return this.get("subfield");
            } else {
                return this.get("field");
            }
        },
        width: function() {
            return this.get("width");
        },
        isSpecial: function() {
            return this.has("special");
        },
        isLink: function() {
            return this.get("special") != undefined && this.get("special") == "link";
        },
        isExpandable: function() {
            return this.get("special") != undefined && this.get("special") == "expandable";
        },
        isSelect: function() {
            return this.get("special") != undefined && this.get("special") == "select";
        },
        isRendered: function() {
            return this.get("special") != undefined && this.get("special") == "rendered";
        },
        isBool : function() {
            return this.get("special") != undefined && this.get("special") == "bool";

        },
        rendering: function(value, mainrow, model) {
            return this.get("rendering")(value, mainrow, model);
        },
        tdclass: function() {
            return this.get('tdclass') || "";
        },
        substyle : function() {
            return this.get('substyle') == undefined ?  "margin-left:10px" : this.get('substyle') ;
        }
    });


    window.Schema = Backbone.Model.extend({
        defaults: {
            sorting: new Sorting({ disabled: true }),
            textfiltering: new TextFiltering({ disabled: true}),
            selectfiltering : [],                              
            paging: new Paging({ disabled: true }),
            options: [],
            extraParams: {},
            expandedByDefault : false
        },
        initialize: function() {
            _.bindAll(this, 'change');
            var schema = this;
          var paging = this.paging();
          // we reset the page to 0 when we change the filtering
          // if we do this first, the right thing happens, otherwise,
          // it goes into infinite loop -- Eric
          this.textfiltering().bind('change', function(){paging.changePage(0);});
          this.textfiltering().bind('change', function() {schema.trigger('change')});
          _.each(this.selectfiltering(), function(f) {
              f.bind('change', function(){ paging.changePage(0);});
              f.bind('change', function() {schema.trigger('change')});
          });
          this.sorting().bind('change', function() {schema.trigger('change')});
          this.paging().bind('change:pageCurrent', function() {schema.trigger('change')});
        },
        cell: function(i) {
            return this.get("cells")[i];
        },
        size: function() {
            return this.get("cells").length;
        },
        allowSelect: function() {
            for (var i = 0; i < this.size(); i++) {
                if (this.cell(i).isSelect()) {
                    return true;
                }
            }
            return false;
        },
        textfiltering: function() {
            return this.get("textfiltering");
        },
        selectfiltering: function() {
            return this.get("selectfiltering");
        },
        sorting: function() {
            return this.get("sorting");
        },
        paging: function() {
            return this.get("paging");
        },
        options: function() {
            return this.get("options");
        },
        optionsAvaible: function() {
            return this.options().length > 0;
        },
        extraParams: function() {
            return this.get("extraParams");
        },
        expandedByDefault : function() {
            return this.get("expandedByDefault") == true;
        },
        url: function() {
            return this.get("url");
        },
        namespace: function() {
            return this.get('namespace');
        },
        initSessionStorageNamespace: function(name) {
            this.set({ namespace: name });
            this.textfiltering().setSessionStorageNamespace(name);
            _.each(this.selectfiltering(), function(f) {f.setSessionStorageNamespace(name)});
            this.sorting().setSessionStorageNamespace(name);
        },
        getSchemaUrlParams: function() {
            var params = this.extraParams();
            params.page = this.paging().pageCurrent();
            params.offset = params.page * this.paging().pageSize();
            params.textfilter = this.textfiltering().text();
            params.selectfilter = JSON.stringify(_.map(this.selectfiltering(),function(f) {return {name: f.name(),value: (f.selected() != undefined) ? f.selected().value : ""}; }));
            params.sort = this.sorting().current();
            params.sortReversed = this.sorting().isAsc();
            return params;
        }
    });

})(window);
