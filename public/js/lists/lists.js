/*
 * This is the main module for all list in our system.
 *
 * Example usage can be found in doclist.st. There is version that
 * uses almost all features offered by this module.
 *
 * Introduction:
 *
 * To use list create KontraList object by using var list =
 * new KontraList({...}) and doing an append of list.el()
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
            _.bindAll(this, 'render', 'renderSelection');
            this.model.bind('change', this.render);
            this.model.bind('selected:change', this.renderSelection);
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
                    this.checkbox.click(function(){view.selectCheck();return false;})
                    elem = this.checkbox;
                } else if (cell.isRendered() && value != undefined) {
                    elem = cell.rendering(value, undefined, this.model);
                } else if (cell.isExpandable() && value != undefined) {
                    elem = $("<a class='expand' />").text(value);
                    elem.click(function() { model.toggleExpand(); return false;});
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
            this.pretableboxleft = $("<div class='col float-left'/>");
            this.pretableboxright = $("<div class='col float-right'/>");
            this.pretablebox = $("<div/>");
            this.pretableboxleft.append(this.emptyAlternative);
            $(this.el).append(this.pretablebox);
            this.pretablebox.append(this.pretableboxleft).append(this.pretableboxright);
        },
        hasFirstTopBox : function() {
            return this.headerExtras != undefined || this.schema.allFiltering().length > 0 || !this.schema.textfiltering().disabled();
        },
        hasSecoundTopBox : function() {
            return this.schema.options().length > 0 || this.schema.actions().length > 0;
        },
        
        prerender: function() {
            var view = this;
            this.pretableboxleft = $("<div class='col float-left'/>");
            this.pretableboxright = $("<div class='col float-right'/>");
              this.pretablebox = $("<div class='tab-content'/>");
            this.tableoptionbox = $("<div class='option-top-box' />");
            this.pretableboxsubbox = $("<div class='subbox empty'/>");
            this.tablebox = $("<div class='table'/>");

            this.tableboxfooter = $("<div/>");
            this.tablebox.append(this.tableboxfooter);
            this.pretablebox = $("<div/>");

            if (this.hasFirstTopBox() || this.hasSecoundTopBox()) {
                this.tableoptionbox = $("<div class='option-top-box' />");
                $(this.el).append(this.tableoptionbox);
                // Top box - for filters
                if (this.hasFirstTopBox()) {
                    this.pretablebox = $("<div/>");
                    this.pretableboxleft = $("<div class='col float-left'/>");
                    this.pretableboxright = $("<div class='col float-right'/>");
                    this.pretablebox.append(this.pretableboxleft).append(this.pretableboxright).append("<div class='clearfix'/>");
                    this.tableoptionbox.append(this.pretablebox);
                    if (this.headerExtras != undefined) {
                        if (typeof(this.headerExtras) == "function") 
                            this.pretableboxleft.append(this.headerExtras());
                        else
                            this.pretableboxleft.append(this.headerExtras);
                    }
                    _.each(this.schema.allFiltering(),function(f) {
                        var filter = new FilteringView({model: f, el: $("<div class='float-left'/>")});
                        view.pretableboxleft.append(filter.el);
                    })
                    if (!this.schema.textfiltering().disabled()) {
                        var filter = new FilteringView({model: this.schema.textfiltering(), el: $("<div style='searchBox height:30px'/>")});
                        this.pretableboxright.append(filter.el);
                    }
                }
                // Secound top box - for actions
                if (this.hasSecoundTopBox())
                {
                    this.pretableboxsubbox = $("<div class='subbox'/>");
                    this.tableoptionbox.append(this.pretableboxsubbox)
                    if (this.schema.actionsAvaible()) {
                        this.prepareActions();
                    }
                    if (this.schema.optionsAvaible()) {
                        this.prepareOptions();
                    }
                }
            }
            $(this.el).append(this.tablebox);
            if (this.bottomExtras != undefined) {
                this.tableboxfooter.append(this.bottomExtras);
            }
            if (!this.schema.paging().disabled()) {
                var pagingFooter = $("<div class='table-paginate'/>");
                new PagingView({model: this.schema.paging(), el: pagingFooter});
                this.tableboxfooter.append(pagingFooter);
            }
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
                                       cssClass: "float-left extra-width",
                                       name : localization.more,
                                       theme : "black",
                                       expandSide : "right"
            });
            console.log("Generating selects for options");
            this.pretableboxsubbox.removeClass('empty');
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
           view.pretableboxsubbox.removeClass('empty');
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
                   var h = $("<span/>");
                   var makeText = function() {
                        var text = cell.name();
                        if (schema.sorting().isSortable(cell.field())) {
                            var field = cell.field();
                            h.click(schema.sorting().sortOnFunction(field));
                            if (schema.sorting().isCurrent(cell.field())) {
                                if (schema.sorting().isAsc()) {
                                    text += " ▼";
                                } else if (schema.sorting().isDesc()) {
                                    text += " ▲";
                                }
                            }
                        }
                        h.html(text);
                     }
                   makeText();
                   this.model.bind('change', makeText);

                   th.append(h);
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
                if (this.emptyAlternative != undefined) this.emptyAlternative.detach();
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
            _.each(this.model.first(this.schema.paging().showLimit()),function(e) {
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

    window.KontraList = function(args) {
            var self = this; 
            var schema = args.schema;
            schema.initSessionStorageNamespace(args.name);
            var model = new List({ schema: schema });
            var view = new ListView({
                model: model,
                schema: schema,
                el: $("<div class='list-container'/>"),
                headerExtras: args.headerExtras,
                bottomExtras: args.bottomExtras,
                emptyAlternative: args.emptyAlternative
            });

            this.model = function() {return model;};
            this.el = function() {return $(view.el);};
            this.recall = function() {
              view.startLoading();
              model.fetch({ data: schema.getSchemaUrlParams(),
                                processData: true,
                                cache: false,
                                success: function() {view.stopLoading(); },
                                error : function() {},
                                timeout: args.timeout
              });
            };
            this.silentFetch = function() {
                model.fetch({ data: schema.getSchemaUrlParams(),
                                processData: true,
                                cache: false,
                                timeout: args.timeout
              });
            } 
            this.model = function() {return model;}
            this.setShowLimit = function(l) {
                    schema.paging().setShowLimit(l);
                    schema.trigger("change");
            };
            schema.bind('change', function() {self.recall();});
            if (args.loadOnInit != false) self.recall();
        }
})(window);
