/*
 *Lists actios
 */


(function(window) {
    window.ListAction= Backbone.Model.extend({
        defaults: {
            avaible: function() {return false;},
            onSelect : function() {return false;},
            name : "",
            visible : false,
            acceptEmpty : false,
            color : "black",
            button : undefined //Custom button, jQuery object. We don't control it's visability.
        },
        initialize: function (args) {
            this.update();
        },
        name: function(name) {
            return this.get("name");
        },
        avaible: function(objs)  {
            var af = this.get("avaible");
            return (objs.length > 0 || this.acceptEmpty()) && _.all(objs, function(o) {return af(o);});
        },
        onSelect: function() {
            return this.get("onSelect")(this.get("list").getSelected());
        },
        visible : function() {
          return this.get("visible");
        },
        acceptEmpty : function() {
          return this.get("acceptEmpty");
        },
        button : function() {
          return this.get("button");
        },
        color : function() {
          return this.get("color");
        },
        update : function() {
            if (this.get('list') == undefined) return;
            this.set({"visible" : this.avaible(this.get("list").getSelected()) }) ;
        }
    });

    window.ListActionView = Backbone.View.extend({
        model: ListAction,
        tagname : 'div',
        initialize: function(args) {
            _.bindAll(this, 'render', 'updateVisability');
            this.model.bind('change', this.updateVisability);
            this.model.update();
            this.render();
        },
        updateVisability : function() {
            if (this.model.visible())
                $(this.el).css("opacity", "1");
            else
                $(this.el).css("opacity", "0.5");
        },
        render: function() {
            var view = this;
            var model = this.model
            this.updateVisability();
            var button = model.button();
            if (button == undefined)
                button = Button.init({
                                      color : model.color(),
                                      size : "tiny",
                                      text  : model.name(),
                                      onClick : function() {if (model.visible()) model.onSelect(); return false;}
                });
            $(this.el).append(button.input());
        }
    });
})(window);
