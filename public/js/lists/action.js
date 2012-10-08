/*
 *Lists actios
 */


(function(window) {
    window.ListAction= Backbone.Model.extend({
        defaults: {
            onSelect : function() {return false;},
            name : "",
            acceptEmpty : false,
            color : "black",
            width : 130, // Default button width
            button : undefined, //Custom button, jQuery object. We don't control it's visability.
            emptyMessage : "", // Message to show when nothing is selected and we dont accept empty
            notAvaibleMessage : "", 
        },
        initialize: function (args) {
        },
        name: function(name) {
            return this.get("name");
        },
        allAvaible: function()  {
            var af = this.get("avaible");
            return _.all(this.selected(), function(o) {return af(o);});
        },
        selected : function() {
          return this.get("list").getSelected();
        },
        onSelect: function() {
          return this.get("onSelect")(this.selected());
        },
        acceptEmpty : function() {
          return this.get("acceptEmpty");
        },
        emptyMessage : function() {
          return this.get("emptyMessage");
        },
        notAvaibleMessage : function() {
          return this.get("notAvaibleMessage");
        },
        button : function() {
          return this.get("button");
        },
        color : function() {
          return this.get("color");
        },
        width : function() {
          return this.get("width");
        }
    });

    window.ListActionView = Backbone.View.extend({
        model: ListAction,
        tagname : 'div',
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.render();
        },
        onSelect : function() {
            var self = this;
            var model = self.model;
            if (model.selected().length == 0 && !model.acceptEmpty())
              FlashMessages.add({color: "red", text : model.emptyMessage()});
            else if (!model.allAvaible())
              FlashMessages.add({color: "red", text : model.notAvaibleMessage()});
            else
              model.onSelect();
            return false;
              
        },
        render: function() {
            var self = this;
            var model = self.model
            var button = model.button();
            if (button == undefined)
                button = Button.init({
                                      color : model.color(),
                                      size : "tiny",
                                      text  : model.name(),
                                      width : model.width(),
                                      onClick : function() { return self.onSelect();}
                });
            $(this.el).append(button.input());
        }
    });
})(window);
