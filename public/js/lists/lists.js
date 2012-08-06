/*
 * This is the main module for all list in our system.
 *
 * Example usage can be found in doclist.st. There is version that
 * uses almost all features offered by this module.
 *
 * Introduction:
 *
 * To use list create KontraList object by using var list =
 * KontraList().init({...}) and doing an append of list.view.el
 * somewhere on page. This is just the jQuery object so this should be
 * easy, but css is probably expecting some more structure around
 * inserted element, so please check this out with example.
 *
 * Most important param of init is the schema object. It defines how
 * the table looks and behaves.
 *
 * Important parts of schema:
 *   url       - where to take data from
 *   cells     - definition of table columns with data binding and l&f details
 *   sorting   - if sorting is active and at what columns
 *   paging    - if paging is available
 *   filtering - if filtering is allowed
 *   options   - If there should be some options (select-like input, top left)
 *
 *  On load or when some changes occur (like new sorting set) this
 *  table will download new data from server (using this url param) as
 *  an JSON object, drop current table content and fill in with
 *  fetched data. Checkout skrivapa.se/docs to see an example.
 *
 *  There are some utils in ListUtils.hs to parse sorting, paging or
 *  searching.
 *
 *  There will be more documetation coming when module is more stable.
 */


(function(window) {
    var ListObject = Backbone.Model.extend({
        defaults: {
            fields: [],
            subfields: [],
            selected: false,
            expanded: false,
            unsaved: false
        },
        initialize: function (args) {
            if (this.collection != undefined && this.collection.schema && this.collection.schema.expandedByDefault())
            {
                this.set({expanded : true});
            }
            if (this.collection != undefined && this.collection.schema != undefined &&  this.field("id") != undefined)
            {
                var namespace = this.collection.schema.namespace();
                var val = SessionStorage.get(namespace, "expanded" + this.field("id"));
                if (val != undefined && val != "")
                this.set({ "expanded": val == "true" });
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
            var val = this.isExpanded();
            var namespace = this.collection.schema.namespace();
            var id = this.field("id");
            SessionStorage.set(namespace, "expanded" + id, "" + !val);
            this.set({ "expanded": !val }, {silent : true});
            this.trigger("change");
            return false;
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
        initialize: function(args) {
            _.bindAll(this, 'render', 'renderSelection', 'updateActions');
            this.model.bind('change', this.render);
            this.model.bind('selected:change', this.renderSelection);
            this.model.bind('selected:change', this.updateActions);
            this.schema = args.schema;
            this.model.view = this;
            this.render();
        },
        render: function() {
            $(this.el).children().detach();
            var view = this;
            var model = this.model;
            var mainrow = $(this.el).first();
            for (var i = 0; i < this.schema.size(); i++) {
                var td = $("<td class='row'></td>");
                var cell = this.schema.cell(i);
                td.addClass(cell.tdclass());
                var value = this.model.field(cell.field());
                var elem = undefined;

                if (cell.isSelect()) {
                    td.click(function(){view.selectCheck(); return false;});
                    this.checkbox = $("<div class='listcheckbox'/>");
                    this.renderSelection();
                    this.updateActions();
                    this.checkbox.click(function(){view.selectCheck();return false;})
                    elem = this.checkbox;
                } else if (cell.isRendered() && value != undefined) {
                    elem = cell.rendering(value, undefined, this.model);
                } else if (cell.isExpandable() && value != undefined) {
                    elem = $("<a href='#'/>").text(value).click(function() {model.toggleExpand(); return false;});
                } else if (cell.isBool()) {
                    if (value) {
                        elem = $("<center />").append( $("<a>&#10003;</a>").attr("href", this.model.link()));
                    }
                } else if (cell.isLink()) {
                    if (value != undefined) {
                        elem = $("<a />").text(value).attr("href",this.model.link());
                    }
                } else if (value != undefined) {
                    elem = $("<span/>").text(value);
                }
                if( elem!=undefined ) {
                    td.append(elem);
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
            this.updateActions();
            return this;
        },
        renderSelection: function() {
            if (this.model.isSelected()) {
                $(this.el).addClass("ui-selected");
                if (this.checkbox != undefined) this.checkbox.addClass("checked");
            } else {
                $(this.el).removeClass("ui-selected");
                if (this.checkbox != undefined) this.checkbox.removeClass("checked");
            }
        },
        updateActions : function() {
            if (this.schema.actionsAvaible())
                _.each(this.schema.actions(), function(a) {a.update(); return false;});
        },
        selectCheck: function() {
          if(!this.model.isSelected())
            this.model.select();
          else
            this.model.unselect();
        }
    });

    var ListView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('reset', this.render);
            this.model.bind('change', this.render);
            this.model.view = this;
            this.schema = args.schema;
            this.headerExtras = args.headerExtras;
            this.bottomExtras = args.bottomExtras;
            this.emptyAlternative = args.emptyAlternative;
            this.prerender();
            this.render();
        },
        startLoading : function() {
            $(this.el).css("opacity", 0.5);
        },
        stopLoading : function() {
            $(this.el).css("opacity",1);
        },
        renderEmpty: function() {
            this.main = $("<div class='tab-container'/>");
            this.pretableboxleft = $("<div class='col float-left'/>");
            this.pretableboxright = $("<div class='col float-right'/>");
            this.pretablebox = $("<div class='tab-content'/>");
            this.pretableboxleft.append(this.emptyAlternative);
            this.main.append(this.pretablebox);
            this.pretablebox.append(this.pretableboxleft).append(this.pretableboxright);
            $(this.el).append(this.main);
        },
        prerender: function() {
            var view = this;
            this.main = $("<div class='tab-container'/>");
            this.pretableboxleft = $("<div class='col float-left'/>");
            this.pretableboxright = $("<div class='col float-right'/>");
              this.pretablebox = $("<div class='tab-content'/>");
            this.tableoptionbox = $("<div class='option-top-box' />");
            this.pretableboxsubbox = $("<div class='subbox'/>");
            this.tablebox = $("<div class='tab-table'/>");
            this.tableboxfooter = $("<div/>");
            this.tablebox.append(this.tableboxfooter);
            this.pretablebox.append(this.pretableboxleft).append(this.pretableboxright).append("<div class='clearfix'/>");
            this.main.append(this.tableoptionbox.append(this.pretablebox).append(this.pretableboxsubbox)).append(this.tablebox);

            if (this.headerExtras != undefined) {
                if (typeof(this.headerExtras) == "function") {
                    this.pretableboxleft.append(this.headerExtras());
                } else {
                    this.pretableboxleft.append(this.headerExtras);
                }
            }
            _.each(this.schema.allFiltering(),function(f) {
                console.log("Generating new view for filtering");
                var filter = new FilteringView({model: f, el: $("<div class='float-left'/>")});
                view.pretableboxleft.append(filter.el);
            })
            
            if (!this.schema.textfiltering().disabled()) {
                console.log("Generating new view for text-filtering");
                var filter = new FilteringView({model: this.schema.textfiltering(), el: $("<div style='searchBox height:30px'/>")});
                this.pretableboxright.append(filter.el);
            }

            if (this.schema.actionsAvaible()) {
                this.prepareActions();
            }
            if (this.schema.optionsAvaible()) {
                this.prepareOptions();
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
                        if (model.hasSelected() || e.acceptEmpty) {
                            return onSelectFunction(model.getSelected());
                        } else {
                            return function() {};
                        }
                    }
                }
                return e;
            });
            var select = new Select({  name: "",
                                       options: options,
                                       cssClass: "float-left",
                                       name : "More",
                                       theme : "standard"
            });
            console.log("Generating selects for options");

            this.pretableboxsubbox.append($("<div class='options-box'/>").append(select.view().el));
        },
        prepareActions : function() {
           var actions = this.schema.actions();
           var model = this.model;
           var view = this
           var div = $("<div class='actions-box'/>");
           _.each(actions, function(a) {
                a.set({"list" : model});
                div.append(new ListActionView({model : a, el : $("<div class='float-left actionButton'>")}).el);
            })
           console.log("Generating buttons for actions");
           view.pretableboxsubbox.append(div);
        },
        renderheader: function() {
            var headline = $("<tr/>");
            var schema = this.schema;
            var view = this;
            for (var i = 0; i < schema.size(); i++) {
               var cell = schema.cell(i);
               var th = $("<th>");
               if (cell.isSelect()) {
                   th.append(this.checkbox = $("<div class='listcheckbox' class='selectall'/>"));
                   th.click(function() {view.toggleSelectAll();return false;});
               } else {
                   var a = $("<a/>");
                   var makeText = function() {
                        var text = cell.name();
                        if (schema.sorting().isSortable(cell.field())) {
                            var field = cell.field();
                            a.click(schema.sorting().sortOnFunction(field));
                            if (schema.sorting().isCurrent(cell.field())) {
                                if (schema.sorting().isAsc()) {
                                    text += " ▼";
                                } else if (schema.sorting().isDesc()) {
                                    text += " ▲";
                                }
                            }
                        }
                        a.html(text);
                     }
                   makeText();
                   this.model.bind('change', makeText);

                   th.append(a);
               }
               if (cell.width() != undefined) {
                   th.css("width", cell.width());
               }
               headline.append(th);
            }
            return $("<thead />").append(headline);
        },
        render: function() {
            console.log("Rendering main list");
            if (this.table != undefined)
                this.table.detach();

            if (this.model.length > 0 || this.emptyAlternative == undefined) {
                this.table = $("<table />");
                this.theader = this.renderheader();
                this.tbody = $("<tbody />");
                this.table.append(this.theader).append(this.tbody);
                if (this.schema.allowSelect()) {
                   this.tbody.addClass("selectable");
                }
                this.tablebox.prepend(this.table);
            }
            else {
                this.tablebox.prepend(this.emptyAlternative);
            }  
            
            var body = this.tbody;
            var odd = true;
            var schema = this.schema
            _.each(this.model.first(this.schema.paging().pageSize()),function(e) {
                if (e.view == undefined)
                  new ListObjectView({
                        model: e,
                        schema: schema,
                        el: $("<tr />")
                    });
                body.append(e.view.el);
                if (odd) {
                     $(e.view.el).addClass("odd");
                }
                else {
                     $(e.view.el).removeClass("odd");
                 }
                 odd = !odd;
                
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
            _.bindAll(this, 'recall');
            this.schema = args.schema;
            this.schema.initSessionStorageNamespace(args.name);
            this.model = new List({
                schema: args.schema
            });
            this.view = new ListView({
                model: this.model,
                schema: args.schema,
                el: $("<div class='list-container'/>"),
                headerExtras: args.headerExtras,
                bottomExtras: args.bottomExtras,
                emptyAlternative: args.emptyAlternative
            });
            this.schema.bind('change', this.recall);
            this.recall();
            return this;
        },
        recall: function() {
            var list = this;
            list.view.startLoading();
            this.model.fetch({ data: this.schema.getSchemaUrlParams(),
                               processData: true,
                               cache: false,
                               success: function() {list.view.stopLoading(); },
                               error : function() {list.recall();},
                               timeout: 6000
            });
        }
    };};
})(window);
