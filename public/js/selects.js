/* Select box - as in top-left of archive*/
  
(function( window){

window.SelectOptionModel = Backbone.Model.extend({
  defaults : {
    onSelect : function(){return false;}    
  },
  initialize: function(args){
  },
  name : function(){
       return this.get("name");
  },
  value : function(){
       return this.get("value");
  },
  selected : function() {
       if (this.get("onSelect")(this.value()) == true)
           this.trigger("done");
  }
});

window.SelectModel = Backbone.Model.extend({
  defaults : {
      name  : "",
      options : [],
      expanded : false,
  },
  initialize: function(args){
      var model = this;
      var options = _.map(args.options,function(e) {
                        if (e.onSelect == undefined)
                            e.onSelect = args.onSelect;
                        var option = new SelectOptionModel(e);
                        option.bind("done", function() {model.set({"expanded" : false});});
                        return option;
                    });
      this.set({"options" : options});
  },
  options: function(){
      return this.get("options");
  },
  name : function(){
       return this.get("name");
  },
  iconClass : function(){
       return this.get("iconClass");
  }, 
  expanded : function(){
       return this.get("expanded");
  },
  theme : function(){
       return this.get("theme");
  },
  textWidth : function(){
       return this.get("textWidth");
  },
  toggleExpand: function() {
       this.set({"expanded" : !this.expanded()});    
  }
});

/* View controls bechavior of real input vs. InfoTextInput model
 * Updates object on focus, blur and change. Sets the grey class for input and fills infotext if needed.
 */
window.SelectOptionView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    clear : function() {
        this.off();
        $(this.el).remove();
    },
    render: function () {
        var model = this.model;
        var a = $("<span/>").text(this.model.name());
        $(this.el).append(a);
        $(this.el).click(function() {model.selected(); return false;})
        return this;
    }
    
});


var SelectView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        var view = this;
        $(this.el).mouseout(function() {
                 setTimeout(function() {view.closeIfNeeded();}, 1000)
              });
        $(this.el).mouseenter(function() {view.enterdate = new Date().getTime();});
        this.render();
    },
    closeIfNeeded : function() {
        if ( this.dead != true 
            && this.model.expanded()
            && new Date().getTime() - this.enterdate > 900
            && $(":hover", this.el).size() == 0
           )
          this.model.toggleExpand();
    },
    button : function() {
        var button = $("<div class='select-button' />");
        button.append("<div class='select-button-left' />");
        var label = $("<div class='select-button-label' />");
        if (this.model.iconClass() != undefined)
            label.append($("<div class='select-icon'>").addClass(this.model.iconClass()));
        else
            label.text(this.model.name());
        if (this.model.textWidth() != undefined)
            label.css("width",this.model.textWidth());
        button.append(label);
        button.append("<div class='select-button-right' />");
        return button;
    },
    clear : function() {
        this.dead;
        _.each(this.model.options(),function(o) {
            if (o.view != undefined)
                o.view.clear();
            o.destroy();
        });
        this.off();
        $(this.el).remove();
    },
    render: function () {
        $(this.el).empty();
        $(this.el).addClass(this.model.theme() + "-theme");
        var view = this;
        var options = $("<ul class='select-opts'/>");
        var model = this.model;
        var button = this.button();
        _.each(model.options(),function(e){
                var li = $("<li/>");
                new SelectOptionView({model : e, el : li});
                options.append(li);
        });

        button.click(function(){
            model.toggleExpand();
        });
        if (model.expanded())
            {
              button.addClass("select-exp");
              options.css("display", "block");
            }
        else
            {
              button.removeClass("select-exp");
              options.css("display", "none");
            }
        $(this.el).append(button).append(options);
        view.close = false;
        return this;
    }
});



window.Select = function(args) {
          var model = new SelectModel ({
                                        options: args.options,
                                        name : args.name,
                                        iconClass : args.iconClass,
                                        onSelect : args.onSelect,
                                        theme : args.theme != undefined ? args.theme : "standard",
                                        textWidth : args.textWidth
                                       });
          var input = $("<div class='select'/>");
          if (args.cssClass!= undefined)
              input.addClass(args.cssClass);
          var view = new SelectView({model : model, el : input});
          return new Object({
              model : function() {return model;},
              view : function()  {return view;},
              clear : function() {view.clear(); model.destroy();}
            });    
};

})(window); 
