/*
 * Filtering options- for now its just text search and persistance
 */


(function(window) {

    window.TextFiltering = Backbone.Model.extend({
        disabled: function() {
            return this.get("disabled") != undefined && this.get("disabled") == true;
        },
        text: function() {
            return this.get("text");
        },
        searchText: function(text) {
            SessionStorage.set(this.get("namespace"), "textfiltering", text);
            this.set({ text: text });
        },
        infotext: function() {
            return this.get("infotext");
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            if (SessionStorage.get(namespace, "textfiltering") != undefined && ! this.disabled()) {
                this.searchText(SessionStorage.get(namespace, "textfiltering"));
            }
        }
    });


    window.TextFilteringView = Backbone.View.extend({
        model: TextFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
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



  window.SelectFiltering = Backbone.Model.extend({
        defaults : {
            options: [],
            selected: undefined,
            description: "",
            name: ""
        },
        initialize: function(args) {
        },
        options: function() {
            return this.get("options");
        },
        description: function() {
            return this.get("description");
        },
        name: function() {
            return this.get("name");
        },
        textWidth: function() {
            return this.get("textWidth");
        }, 
        selected: function() {
            return this.find(this.get("selected"));
        },
        find : function(v){
            return _.filter(this.options(), function(o) {return o.value == v})[0];
        },
        select: function(v) {
            if (this.find(v) != undefined) {
               SessionStorage.set(this.get("namespace"), "select-filtering" + this.name(), this.find(v).value );
               this.set({"selected" : this.find(v).value});
            }
            else
                this.deselect();
                 
        },
        deselect: function() {
            SessionStorage.set(this.get("namespace"), "select-filtering" + this.name(), "" );
            this.set({"selected" : undefined});
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            var fss = SessionStorage.get(namespace, "select-filtering" + this.name());
            if (fss!= undefined)
                this.select(fss);
        }
    });


    window.SelectFilteringView = Backbone.View.extend({
        model: SelectFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function() {view.render();})
            this.render();
        },
        render: function() {
            $(this.el).children().detach();
            var selectFiltering = this.model;
            var name = this.model.description();
            var options = this.model.options();

            if (selectFiltering.selected() != undefined)
            {
                var selected = selectFiltering.selected()
                name = selected.name;
                var options_ = [];
                options_.push({name:  this.model.description(), value: "", onSelect: function(value) {selectFiltering.deselect();} });
                for(var i=0;i< options.length;i++)
                    if (options[i] != selected)
                         options_.push(options[i])
                options = options_;         
                
            }
            var select = new Select({
                options : options,
                name : name,
                theme : selectFiltering.selected() != undefined ? "dark" : "warm",
                textWidth : selectFiltering.textWidth(),
                onSelect : function(value) {
                    selectFiltering.select(value);
                }
            });
            $(this.el).empty().append(select.view().el);
        }
    });



  window.AdvancedSelectFiltering = Backbone.Model.extend({
        defaults : {
            selectfilterings : [],
            on : false
        },
        initialize: function(args) {
        },
        toogle: function() {
            var val = !this.on();
            SessionStorage.set(this.get("namespace"), "advanced-filtering", "" + val);
            this.set({"on" : val});
        },
        on : function() {
            return this.get("on") == true;
        },
        selectfilterings : function() {
            return this.get("selectfilterings");
        },
        setSessionStorageNamespace: function(namespace) {
            this.set({ namespace: namespace });
            var fss = SessionStorage.get(namespace, "advanced-filtering");
            if ((fss == "true" && this.get("on") == false) || fss == "false" && this.get("on") == true)
                this.toogle();
        }
    });
  
    window.AdvancedSelectFilteringToogleView = Backbone.View.extend({
        model: AdvancedSelectFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function() {view.render();})
            this.render();
        },
        render: function() {
            var model = this.model;
            $(this.el).empty();
            var a = $("<a href='#'/>");
            a.click(function() {model.toogle();return false;});
            if (!this.model.on())
                a.text("Show advanced search");
            else
                a.text("Hide advanced search");
            $(this.el).append(a);
        }
    });
    
    window.AdvancedSelectFilteringView = Backbone.View.extend({
        model: AdvancedSelectFiltering,
        initialize: function(args) {
            _.bindAll(this, 'render');
            var view = this;
            this.model.bind('change', function() {view.render();})
            this.render();
        },
        render: function() {
            var model = this.model;
            var container = $("<div class='advanced-search-box'/>");
            $(this.el).children().detach();
            if (!model.on()) return;
            $(this.el).append(container);
            var tr = $("<tr/>");
            container.append($("<table/>").append($("<tbody/>").append(tr)))
            _.each(model.selectfilterings(), function(f) {
                tr.append($("<td/>").append(new SelectFilteringView({model: f, el: $("<div class='advanced-select'/>")}).el));
            })
            return;
        }
    });
})(window);
