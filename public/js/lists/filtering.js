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
        prefix: function() {
            return this.get("prefix");
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
            $(this.el).empty();
            var selectFiltering = this.model;
            var name = this.model.description();
            var options = this.model.options();

            if (selectFiltering.selected() != undefined)
            {
                var selected = selectFiltering.selected()
                name = selected.name;
                if (this.model.prefix() != undefined)
                    name = this.model.prefix() + name;
                var options_ = [];
                options_.push({name:  this.model.description(), value: "", onSelect: function(value) {selectFiltering.deselect();} });
                for(var i=0;i< options.length;i++)
                    if (options[i] != selected)
                         options_.push(options[i])
                options = options_;         
                
            }
            if (this.select != undefined)
                this.select.clear();
            this.select = new Select({
                options : options,
                name : name,
                theme : selectFiltering.selected() != undefined ? "dark" : "warm",
                textWidth : selectFiltering.textWidth(),
                onSelect : function(value) {
                    selectFiltering.select(value);
                }
            });
            $(this.el).append(this.select.view().el);
        }
    });

})(window);
