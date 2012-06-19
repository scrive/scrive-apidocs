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
        selected: function() {
            return this.get("selected");
        },
        description: function() {
            return this.get("description");
        },
        name: function() {
            return this.get("name");
        },
        isSelected : function(o) {
            return this.set({"selected" : undefined} );
        },
        select: function(n) {
           SessionStorage.set(this.get("namespace"), "select-filtering" + this.value(), n );
           this.set({"selected" : _.filter(this.options(), function() {return this.name == n})[0]} );
        },
        deselect: function(o) {
            SessionStorage.set(this.get("namespace"), "select-filtering" + this.value(), "" );
            this.set({"selected" : _.without(this.selected(),[o])});
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
            this.render();
        },
        render: function() {
            var selectFiltering = this.model;
            var select = Select.init({
                options : this.model.options(),
                name : this.model.description(),
                onSelect : function(value) {
                    selectFiltering.select(value);
                }
            })
            $(this.el).empty();
            $(this.el).append(select.input());;
        }
    });



    
})(window);
