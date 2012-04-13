/* This is the main module for all list in our system.
 * Example usage can be found in doclist.st. There is version tat uses almoust all futures offered by this module.
 *
 * Introduction:
 *   To use list create KontraList object by using var list = KontraList().init({...}) and doing an append of
 *  list.view.el somewere on page. This is just the jQuery object so this should be easy, but css is brobably expecting
 *  some more structure around insered elem, so please check this out with example.
 *
 *  Most important param of init is schema object. It defines how the table looks and behaves.
 *  Important parts of schema:
 *      cells - definition of table columns with data binding and l&f details
 *      url - where to take data from
 *      sorting - if sorting is active and at what columns
 *      paging - if paging is avaible
 *      filtering - if filtering is alloud
 *      options - If there should be some options (select-like input, top left)
 *
 *  On load or when some changes occur (like new sorting set) this table will download new data from server (using this url param)
 *  as an JSON object, drop current table content and fill in with fetched data. Checkout skrivapa.se/docs to see an example.
 *
 *  There are some utils in ListUtils.hs to parse sorting, paging or
 *
 *  There will be more documetation coming when module is more stable
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

    window.Sorting = Backbone.Model.extend({
        defaults: {
            fields: [],
            current: undefined,
            order: true
        },
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
        },
        fields: function() {
            return this.get("fields");
        },
        isCurrent: function(field) {
            return this.get("current") != undefined && this.get("current") == field;
        },
        current: function() {
            return this.get("current");
        },
        isAsc: function() {
            return this.get("order") != undefined && this.get("order") == true;
        },
        isDesc: function() {
            return this.get("order") != undefined && this.get("order") == false;
        },
        isSortable: function(field) {
            return _.any(this.fields(), function(f) { return f == field; });
        },
        sortOn: function(field) {
            if (this.isCurrent(field)) {
                this.set({ order: !this.get("order") });
                this.updateSessionStorage();
            } else {
                this.set({order: true, current: field});
                this.updateSessionStorage();
            }
        },
        sortOnFunction: function(field) {
            var sorting = this;
            return function() { sorting.sortOn(field); };
        },
        updateSessionStorage: function() {
            SessionStorage.set(this.get("namespace"), "sorting", this.current());
            SessionStorage.set(this.get("namespace"), "sorting_order", this.isAsc() + "");
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({namespace: namespace});
            if (SessionStorage.get(namespace, "sorting") != undefined &&
                   SessionStorage.get(namespace, "sorting_order") != undefined &&
                   !this.disabled()) {
                    this.set({
                        current: SessionStorage.get(namespace, "sorting"),
                        order: SessionStorage.get(namespace, "sorting_order") == "true"
                    });
            }
        }
    });

    window.Filtering = Backbone.Model.extend({
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
        },
        text: function() {
            return this.get("text");
        },
        searchText: function(text) {
            SessionStorage.set(this.get("namespace"), "filtering", text);
            this.set({ text: text });
        },
        infotext: function() {
            return this.get("infotext");
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            if (SessionStorage.get(namespace, "filtering") != undefined && ! this.disabled()) {
                this.searchText(SessionStorage.get(namespace, "filtering"));
            }
        }
    });

    window.Paging = Backbone.Model.extend({
        defaults: {
            itemMin: 0,
            itemMax: 0,
            itemTotal: 0,
            pageCurrent: 0,
            pageMax: 0
        },
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
        },
        itemMin: function() {
            return this.get("itemMin");
        },
        itemMax: function() {
            return this.get("itemMax");
        },
        itemTotal: function() {
            return this.get("itemTotal");
        },
        pageCurrent: function() {
            return this.get("pageCurrent");
        },
        pageMax: function() {
            return this.get("pageMax");
        },
        changePage: function(i) {
            this.set({ "pageCurrent": i });
        },
        changePageFunction: function(i) {
            var paging = this;
            return function() { paging.changePage(i); };
        },
        updateWithServerResponse: function(resp) {
            this.set(resp);
            if (this.pageCurrent() > this.pageMax()+1)
                this.changePage(this.pageMax()+1);
        }
    });

    var PagingView = Backbone.View.extend({
        model: Paging,
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.render();
        },
        render: function() {
            var paging = this.model;
            var main = $("<div class='pages'>");
            var items = $("<div />");
            items.append($("<strong />").text((paging.itemMin() + 1) + " - " + (paging.itemMax() + 1) + " " + localization.of + " " + paging.itemTotal()));
            var pages = $("<div />");
            for (var i = 0; i <= paging.pageMax(); i++) {
                if (i == paging.pageCurrent()) {
                    pages.append($("<span class='page-change current'>" + (i + 1) + "</span>"));
                } else {
                    var a = $("<span class='page-change'>" + (i + 1) + "</span>");
                    a.click(paging.changePageFunction(i));
                    pages.append(a);
                }
            }
            if (paging.itemMax() >= paging.itemMin()) {
                main.append(items);
            }
            if (paging.pageMax() > 0) {
                main.append(pages);
            }
            $(this.el).append(main);
        }
    });

    var FilteringView = Backbone.View.extend({
        model: Filtering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.render();
        },
        render: function() {
            var filtering = this.model;
            var searchBox = InfoTextInput.init({ infotext: filtering.infotext(),
                                                 value: filtering.text(),
                                                 cssClass: "list-search float-right" });
            searchBox.input().keypress(function(event) {
                var keycode = (event.keyCode ? event.keyCode : event.which);
                if (keycode == '13') {
                    filtering.searchText($(this).val());
                }});
            var button = Button.init({color: "black",
                                      size: "tiny",
                                      text: localization.searchBoxButtonText,
                                      cssClass: "float-right",
                                      onClick: function() {
                                          filtering.searchText(searchBox.value());
                                      }
                                     });
            $(this.el).append(button.input()).append(searchBox.input());
        }
    });

    window.Schema = Backbone.Model.extend({
        defaults: {
            sorting: new Sorting({ disabled: true }),
            filtering: new Filtering({ disabled: true}),
            paging: new Paging({ disabled: true }),
            options: [],
            extraParams: {}
        },
        initialize: function() {
            _.bindAll(this, 'change');
            var schema = this;
          var paging = this.paging();
          // we reset the page to 0 when we change the filtering
          // if we do this first, the right thing happens, otherwise,
          // it goes into infinite loop -- Eric
          this.filtering().bind('change', function(){paging.changePage(0);});

            this.filtering().bind('change', function() {schema.trigger('change')});
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
        filtering: function() {
            return this.get("filtering");
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
        url: function() {
            return this.get("url");
        },
        namespace: function() {
            return this.get('namespace');
        },
        initSessionStorageNamespace: function(name) {
            this.set({ namespace: name });
            this.filtering().setSessionStorageNamespace(name);
            this.sorting().setSessionStorageNamespace(name);
        },
        getSchemaUrlParams: function() {
            var params = this.extraParams();
            params.page = this.paging().pageCurrent();
            params.filter = this.filtering().text();
            params.sort = this.sorting().current();
            params.sortReversed = this.sorting().isAsc();
            return params;
        }
    });

    var ListObject = Backbone.Model.extend({
        defaults: {
            fields: [],
            subfields: [],
            selected: false,
            expanded: false,
            unsaved: false
        },
        initialize: function (args) {
            if (this.collection != undefined && this.collection.schema != undefined &&  this.field("id") != undefined)
            {
                var namespace = this.collection.schema.namespace();
                this.set({ "expanded": SessionStorage.get(namespace, "expanded" + this.field("id")) == "true" });
            }
        },
        field: function(name) {
            return this.get("fields")[name];
        },
        subfield: function(idx, name)  {
            return this.get("subfields")[idx][name];
        },
        subfieldsSize: function() {
            return this.get("subfields").length;
        },
        hasLink: function() {
            return this.get("link") != undefined;
        },
        link: function() {
            return this.get("link");
        },
        isSelected: function() {
            return this.get("selected") == true;
        },
        isUnsaved: function() {
            return this.get("unsaved");
        },
        select: function() {
            this.set({ "selected": true }, {silent: true});
            this.trigger("selected:change");
        },
        unselect: function() {
            this.set({ "selected": false }, {silent: true});
            this.trigger("selected:change");
        },
        isExpanded: function() {
            return this.get("expanded") == true;
        },
        toggleExpand: function() {
            console.log("toggling");
            var val = this.isExpanded();
            var namespace = this.collection.schema.namespace();
            var id = this.field("id");
            SessionStorage.set(namespace, "expanded" + id, "" + !val);
            this.set({ "expanded": !val }, {silent : true});
            this.trigger("change");
        }
    });

    var List = Backbone.Collection.extend({
        model: ListObject,
        initialize: function(args) {
            this.schema = args.schema;
            this.url = args.schema.url();
        },
        getSelected: function() {
            return this.filter(function(e) { return e.isSelected(); });
        },
        isEmpty: function() {
            return this.models.length === 0;
        },
        hasUnselected: function() {
            var l = this.models.length;
            for (var i = 0; i < l; i++) {
                if (!this.models[i].isSelected()) {
                    return true;
                }
            }
            return false;
        },
        hasSelected: function() {
            var l = this.models.length;
            for (var i = 0; i < l; i++) {
                if (this.models[i].isSelected()) {
                    return true;
                }
            }
            return false;
        },
        parse: function(response) {
            this.schema.paging().updateWithServerResponse(response.paging);
            return response.list;
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
        }
    });

    var ListObjectView = Backbone.View.extend({
        model: ListObject,
        events: {
            'click .selectme': 'selectCheck',
            'click .expand': 'toggleExpand',
            'click .row': 'selectRow'
        },
        initialize: function(args) {
            _.bindAll(this, 'render', 'renderSelection');
            this.model.bind('change', this.render);
            this.model.bind('selected:change', this.renderSelection);
            this.schema = args.schema;
            this.model.view = this;
            this.render();
        },
        render: function() {
            $(this.el).empty();
            var mainrow = $(this.el).first();
            for (var i = 0; i < this.schema.size(); i++) {
                var td = $("<td class='row'></td>");
                var cell = this.schema.cell(i);
                td.addClass(cell.tdclass());
                var value = this.model.field(cell.field());
                if (cell.isSpecial()) {
                    if (cell.isSelect()) {
                        td.append(this.checkbox = $("<input type='checkbox' class='selectme check'/>"));
                    } else if (cell.isRendered() && value != undefined) {
                        td.append(cell.rendering(value, undefined, this.model));
                    } else if (cell.isExpandable() && value != undefined) {
                        td.html($("<a href='#' class='expand'>" + value + "</a>"));
                    } else if (cell.isBool()) {
                        if (value)  td.append("<center><a href='" + this.model.link() + "'>&#10003;</a></center>");
                    } else
                            td.append($("<a href='" + this.model.link() + "'>" + value + "</a>"));
                    }
                   else if (value != undefined) {
                        var span = $("<span >" + value + "</span>");
                        td.html(span);
                    }
                mainrow.append(td);
            }
            if (this.model.isExpanded()) {
             if ($(this.el).size() === 1) {
                    for (var j = 0; j < this.model.subfieldsSize(); j++) {
                        this.el = $(this.el).add($("<tr />"));
                    }
             }

             for (var j = 0; j < this.model.subfieldsSize(); j++) {
                var subrow = $(this.el).eq(j + 1);
                for (var i = 0; i < this.schema.size(); i++) {
                    var div = $("<div/>").attr('style',this.schema.cell(i).substyle());
                    var td = $("<td></td>").append(div);
                    var value = this.model.subfield(j, (this.schema.cell(i).subfield()));
                    if (value != undefined) {
                        if (this.schema.cell(i).isRendered()) {
                            div.append(this.schema.cell(i).rendering(value, j, this.model));
                        }
                        else if (this.schema.cell(i).isSelect()) {
                        }
                        else {
                            div.text(value);
                        }
                    }
                    subrow.append(td);
                }
             }
            }
            this.renderSelection();
            return this;
        },
        renderSelection: function() {
            if (this.model.isSelected()) {
                $(this.el).addClass("ui-selected");
                if (this.checkbox != undefined) {
                    this.checkbox.attr("checked", "true");
                }
            } else {
                $(this.el).removeClass("ui-selected");
                if (this.checkbox != undefined) {
                    this.checkbox.removeAttr("checked");
                }
            }
        },
        selectCheck: function(e) {
          if(e.target.checked)
            this.model.select();
          else
            this.model.unselect();
        },
        selectRow: function(e) {
            // ignore checkboxes and links
            if (!$(e.target).is(":checkbox, a")) {
                // select only this one
                this.model.collection.selectNone();
                this.model.select();
            }
        },
        toggleExpand: function() {
            this.model.toggleExpand();
            return false;
        }
    });

    window.Loading = Backbone.Model.extend({
        defaults: {
          loading: true,
          colcount: 0
        },
        initialize: function(args) {
          this.set({ colcount: args.schema.size() });
        },
        isLoading: function() {
            return this.get("loading");
        },
        start: function() {
            this.set({ loading: true });
        },
        stop: function() {
            this.set({ loading: false });
        },
        colCount: function() {
            return this.get("colcount");
        }
    });

    window.LoadingView = Backbone.View.extend({
        model: Loading,
        initialize: function(args) {
            _.bindAll(this, 'render', 'makeLoader', 'removeLoader');
            this.model.bind('change', this.render);
            this.model.view = this;
            this.colcount = args.colcount;
            this.render();
        },
        render: function() {
          if (this.model.isLoading() && !this.isLoader()) {
            var img = $("<img src='/theme/images/wait30trans.gif' style='margin:30px'>");
            var overlay = $("<div>");
            overlay.append(img);

            //some css stuff that centres the load icon in the overlay
            //on top of the background this.el
            overlay.css("position", "absolute");
            overlay.css("margin", "auto");
            overlay.css("height", "90px");
            overlay.css("top", "0px");
            overlay.css("bottom", "0px");
            overlay.css("left", "0px");
            overlay.css("right", "0px");
            overlay.css("text-align", "center");
            overlay.css("display", "none");

            $(this.el).css("position", "relative");
            this.makeLoader(overlay);

            this.loader.fadeIn(2000);
            $(this.el).css("opacity", 0.5);
            //this.el.css("opacity", "0.5");
          } else if (!this.model.isLoading() && this.isLoader()) {
            this.removeLoader();
            $(this.el).fadeTo(500, 1.0);
          }
        },
        isLoader: function() {
          return this.loader != undefined;
        },
        makeLoader: function(elem) {
          this.loader = elem;
          $(this.el).prepend(this.loader);
        },
        removeLoader: function() {
          this.loader.remove();
          this.loader = undefined;
        }
    });

    var ListView = Backbone.View.extend({
        events: {
            "click .selectall": "toggleSelectAll"
        },
        initialize: function(args) {
            _.bindAll(this, 'render', 'makeElementsViews', 'toggleSelectAll');
            this.model.bind('reset', this.makeElementsViews);
            this.model.bind('change', this.render);
            this.model.view = this;
            this.schema = args.schema;
            this.loading = args.loading;
            this.headerExtras = args.headerExtras;
            this.bottomExtras = args.bottomExtras;
            this.makeElementsViews([]);
        },
        makeElementsViews : function(ms){
            $(this.el).children().detach();
            this.prerender();
            new LoadingView({
                model: this.loading,
                colcount: this.schema.size(),
                el: this.table
            });
            for (var i = 0; i < ms.length; i++) {
                new ListObjectView({
                    model: ms.at(i),
                    schema: this.schema,
                    el: $("<tr />")
                });
            }
            this.render();
        },
        prerender: function() {
            this.main = $("<div class='tab-container'/>");
            this.pretableboxleft = $("<div class='col float-left'/>");
            this.pretableboxright = $("<div class='col float-right'/>");
            this.pretablebox = $("<div class='tab-content'/>");
            this.tablebox = $("<div class='tab-table'/>");
            this.table = $("<table />");
            this.theader = this.prerenderheader();
            this.tbody = $("<tbody>");
            this.tableboxfooter = $("<div/>");
            this.table.append(this.theader).append(this.tbody);
            this.tablebox.append(this.table).append(this.tableboxfooter);
            this.pretablebox.append(this.pretableboxleft).append(this.pretableboxright).append("<div class='clearfix'/>");
            this.main.append(this.pretablebox).append(this.tablebox);

            if (this.schema.optionsAvaible()) {
                this.prepareOptions();
            }
            if (this.headerExtras != undefined) {
                if (typeof(this.headerExtras) == "function") {
                    this.pretableboxleft.append(this.headerExtras());
                } else {
                    this.pretableboxleft.append(this.headerExtras);
                }
            }
            if (!this.schema.filtering().disabled()) {
                var searchBox = $("<div class='searchBox float-right'/>");
                new FilteringView({model: this.schema.filtering(), el: searchBox});
                this.pretableboxright.append(searchBox);
            }
            if (this.schema.allowSelect()) {
                this.tbody.addClass("selectable");
            }
            if (this.bottomExtras != undefined) {
                this.tableboxfooter.append(this.bottomExtras);
            }
            if (!this.schema.paging().disabled()) {
                var pagingFooter = $("<div class='table-paginate'/>");
                new PagingView({model: this.schema.paging(), el: pagingFooter});
                this.tableboxfooter.append(pagingFooter);
            }
            $(this.el).append(this.main);
        },
        prepareOptions: function() {
            var options = this.schema.options();
            var model = this.model;
            options = _.map(options, function(e) {
                var onSelectFunction = e.onSelect;
                if (onSelectFunction != undefined) {
                    e.onSelect = function() {
                        if (model.hasSelected()) {
                            return onSelectFunction(model.getSelected());
                        } else {
                            return function() {};
                        }
                    }
                }
                return e;
            });
            var select = Select.init({ name: localization.select,
                                       options: options,
                                       cssClass: "float-left"});
            this.pretableboxleft.append(select.input());
        },
        prerenderheader: function() {
            var headline = $("<tr/>");
            for (var i = 0; i < this.schema.size(); i++) {
               var cell = this.schema.cell(i);
               var th = $("<th>");
               if (cell.isSpecial() && cell.isSelect()) {
                   th.append(this.checkbox = $("<input type='checkbox' class='selectall'>"));
               } else {
                   var a = $("<a/>");
                   var text = cell.name();
                   if (this.schema.sorting().isSortable(cell.field())) {
                       var field = cell.field();
                       var schema = this.schema;
                       a.click(schema.sorting().sortOnFunction(field));
                       if (this.schema.sorting().isCurrent(cell.field())) {
                           if (this.schema.sorting().isAsc()) {
                               text += " ▼";
                           } else if (this.schema.sorting().isDesc()) {
                               text += " ▲";
                           }
                       }
                   }
                   th.append(a.html(text));
               }
               if (cell.width() != undefined) {
                   th.css("width", cell.width());
               }
               headline.append(th);
            }
            return $("<thead />").append(headline);
        },
        render: function() {
            //We mark header checkbox only if one is defined and all elements are selected
            if (this.checkbox != undefined) {
                if (this.model.hasUnselected() || this.model.isEmpty()) {
                    this.checkbox.attr('checked', false);
                } else {
                    this.checkbox.attr('checked', true);
                }
            }
            var body = this.tbody;
            var odd = true;
            this.model.forEach(function(e) {
                if (e.view != undefined) {
                    body.append($(e.view.el));
                    if (odd) {
                        $(e.view.el).addClass("odd");
                    }
                    odd = !odd;
                }
            });
            return this;
        },
        toggleSelectAll: function() {
            this.model.off('change');
            if (this.model.hasUnselected()) {
                this.model.selectAll();
            } else {
                this.model.selectNone();
            }
            this.model.bind('change', this.render);
        }
    });

    window.KontraList = function() { return {
        init: function(args) {
            _.bindAll(this, 'recall', 'beforeFetch', 'afterFetch');
            this.schema = args.schema;
            this.loading = new Loading({
                schema: this.schema
            });
            this.schema.initSessionStorageNamespace(args.name);
            this.model = new List({
                schema: args.schema
            });
            this.view = new ListView({
                model: this.model,
                schema: args.schema,
                loading: this.loading,
                el: $("<div class='list-container'/>"),
                headerExtras: args.headerExtras,
                bottomExtras: args.bottomExtras
            });
            this.schema.bind('change', this.recall);
            this.recall();
            return this;
        },
        beforeFetch: function() {
            this.loading.start();
        },
        afterFetch: function() {
            this.loading.stop();
        },
        recall: function() {
            var list = this;
            this.beforeFetch();
            this.model.fetch({ data: this.schema.getSchemaUrlParams(),
                               processData: true,
                               cache: false,
                               success: this.afterFetch,
                               error: function() {
                                 console.error("Failed to fetch list, trying again ...");
                                 window.setTimeout(list.recall, 1000);
                               }
            });
        }
    };};
})(window);
