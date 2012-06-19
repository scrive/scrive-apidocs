/*
 * Filtering options- for now its just text search and persistance
 */


(function(window) {

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


    window.FilteringView = Backbone.View.extend({
        model: Filtering,
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

})(window);
