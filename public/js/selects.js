/* Select box - as in top-left of archive*/

(function( window){

window.SelectOptionModel = Backbone.Model.extend({
  defaults : {
      onSelect : function(){return false;},
      style : ""
  },
  name : function(){
       return this.get("name");
  },
  value : function(){
       return this.get("value");
  },
  style: function() {
       return this.get("style");
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
      expandSide : "left",
      expanded : false,
      onOpen : function() {return true;},
      color: undefined,
      border: undefined,
      style : "",
      cssClass : ""

  },
  initialize: function(args){
      var model = this;
      //Change static options to SelectOptionModel. Propagate onSelect function if this is needed
      var options = _.map(args.options,function(e) {
                        e.onSelect = e.onSelect || args.onSelect;
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
  setName: function(name) {
      this.set({name:name});
      return this;
  },
  style : function(){
       return this.get("style");
  },
  expanded : function(){
       return this.get("expanded");
  },
  expandSide : function() {
       return this.get("expandSide");
  },
  textWidth : function(){
       return this.get("textWidth");
  },
  toggleExpand: function() {
     if (this.expanded())
       this.set({"expanded" : false});
     else
       this.expand();
  },
  expand : function() {
     if (!this.expanded() && this.onOpen() && this.options().length > 0)
       this.set({"expanded" : true});
  },
  onOpen : function(){
       if (this.get("onOpen") != undefined)
        return this.get("onOpen")();
       return true;
  },
  color : function() {
     return this.get("color");
  },
  border : function() {
     return this.get("border");
  },
  cssClass : function() {
     return this.get("cssClass");
  },
  style : function() {
     return this.get("style");
  }
});


window.SelectOptionView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {
        var model = this.model;
        var a = $("<span/>").text(model.name()).attr("style", model.style());
        $(this.el).append(a).click(function() {model.selected(); return false;});
    }

});


var SelectView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        var view = this;
        $(this.el).mouseout(function() {
                 setTimeout(function() {view.closeIfNeeded();}, 100);
              });
        $(this.el).mouseenter(function() {view.enterdate = new Date().getTime();});
        this.render();
    },
    closeIfNeeded : function() {
        if ( this.dead != true
            && this.model.expanded()
            && new Date().getTime() - this.enterdate > 50
            && (this.expButton == undefined || $(":hover", this.expButton).size() == 0)
            && (!BrowserInfo.doesNotSupportHoverPseudoclassSelector() && !BrowserInfo.isPadDevice())
           )
          ;//this.model.toggleExpand();
    },
    button : function() {
        var button = $("<div class='select-button' />");
        button.append("<div class='select-button-left' />");
        var label = $("<div class='select-button-label' />");
        label.attr("style", this.model.style());
        label.text(this.model.name());
        if (this.model.textWidth() != undefined)
            label.css("width",this.model.textWidth());
        button.append(label);
        button.append("<div class='select-button-right' />");
        return button;
    },
    render: function () {
        var model = this.model;
        var view = this;

        if (this.expButton != undefined) $(this.expButton).detach();

        if (!model.expanded()) {
          $(this.el).empty();
          $(this.el).addClass(this.model.cssClass());
          $(this.el).attr("style",this.model.style());
          var button = this.button();
          if (model.border() != undefined) button.css("border", this.model.border());
          $(this.el).append(button);
          button.click(function(){
                    model.toggleExpand();
                    return false;
         });
        }
        else
            {
              this.expButton = $("<div class='select select-exp'/>");
              this.expButton.addClass(this.model.cssClass());
              this.expButton.attr("style",this.model.style());
              this.expButton.css("position","absolute");
              var button = this.button()
              this.expButton.append(button);
              if (model.color()) this.expButton.css('color',model.color());

              $('.select-button',this.expButton).addClass("select-exp").css('z-index',5002);
              this.expButton.css('z-index',5000);
              this.expButton.css('left',$(this.el).offset().left + "px").css('top',($(this.el).offset().top + "px"));
              this.expButton.css("display", "block");
              $('body').append(this.expButton);
              this.expButton.mouseout(function() {
                 setTimeout(function() {view.closeIfNeeded();}, 100);
              });
              this.expButton.mouseenter(function() {view.enterdate = new Date().getTime();});
              var options = $("<ul class='select-opts'/>").addClass(this.model.expandSide());
              if (model.border() != undefined) {
                options.css("border", this.model.border());
                this.expButton.css("border", this.model.border())
              }

            _.each(model.options(),function(e){
                    var li = $("<li/>");
                    if (model.color())
                      li.css('color',model.color());
                    new SelectOptionView({model : e, el : li});
                    options.append(li);
            });

              this.expButton.append(options.css('z-index',5001));
            }
        return this;
    }
});



window.Select = function(args) {
          // Build model
          var model = new SelectModel (args);

          // Build view
          var div = $("<div class='select'/>");
          var view = new SelectView({model : model, el : $("<div class='select'/>")});

          // Export interface
          this.setName = function(name) { model.setName(name);}
          this.open = function(name) { model.expand();}
          this.el = function() {return $(view.el);};

};

})(window);
