/* Select box - as in top-left of archive*/

(function( window){

window.SelectOptionModel = Backbone.Model.extend({
  defaults : {
      onSelect : function(){return false;},
      leftMargin: 0
  },
  initialize: function(args){
  },
  name : function(){
       return this.get("name");
  },
  value : function(){
       return this.get("value");
  },
  extraAttrs: function() {
       return this.get('extraAttrs');
  },
  selected : function() {
       if (this.get("onSelect")(this.value()) == true)
           this.trigger("done");
  },
  leftMargin: function() {
        return this.get('leftMargin');
  },
  offset: function() {
        return this.get('offset');
  }
});

window.SelectModel = Backbone.Model.extend({
  defaults : {
      name  : "",
      options : [],
      expandSide : "left",
      expanded : false,
      onOpen : function() {return true;},
      extraNameAttrs: {},
      expandOnHover : false,
      zIndex : 5000

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
  setName: function(name) {
      this.set({name:name});
      return this;
  },
  extraNameAttrs : function(){
       return this.get("extraNameAttrs");
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
  zIndex : function() {
      return this.get("zIndex");
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
    unexpand: function() {
        this.set({expanded:false});
    },
  expandOnHover : function() {
     return this.get("expandOnHover");
  },
  onOpen : function(){
       if (this.get("onOpen") != undefined)
        return this.get("onOpen")();
       return true;
  },
    offset: function() {
        return this.get('offset');
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
        var a = $("<span/>").html(this.model.name());
        a.css({'margin-left':this.model.leftMargin()});
        if(model.offset()!==undefined) {
            a.css('display', 'block');
            a.css('margin-top', model.offset());
        }
        var attrs = model.extraAttrs() ? model.extraAttrs() : {};
        $.each(attrs, function(attr, val) {
          a.attr(attr, val);
        });
        $(this.el).append(a);
        $(this.el).click(function() {model.selected(); return false;});
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
          this.model.toggleExpand();
    },
    button : function() {
        var button = $("<div class='select-button' />");
        button.append("<div class='select-button-left' />");
        var label = $("<div class='select-button-label' />");
        var extraNameAttrs = this.model.extraNameAttrs() != null ? this.model.extraNameAttrs() : {};
        $.each(extraNameAttrs, function(attr, val) {
          label.attr(attr, val);
        });
        if (this.model.iconClass() != undefined)
            label.append($("<div class='select-icon'>").addClass(this.model.iconClass()));
        else
            label.html(this.model.name());
        if (this.model.textWidth() != undefined)
            label.css("width",this.model.textWidth());
        if(this.model.offset())
            label.css('margin-top', this.model.offset());
        button.append(label);
        button.append("<div class='select-button-right' />");
        return button;
    },
    clear : function() {
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
        var options = $("<ul class='select-opts'/>").addClass(this.model.expandSide());
        options.addClass(this.model.theme() + "-theme");
        var model = this.model;
        var button = this.button();
        _.each(model.options(),function(e){
                var li = $("<li/>");
                new SelectOptionView({model : e, el : li});
                options.append(li);
        });

        button.click(function(){
              model.toggleExpand();
              return false;
        });

        if (model.expandOnHover() && !BrowserInfo.doesNotSupportHoverPseudoclassSelector() && !BrowserInfo.isPadDevice()){
          button.mouseenter(function(){
              view.enterdate = new Date().getTime();
              model.expand();
              setTimeout(function() {view.closeIfNeeded();}, 100);
              return false;
          });
        }
        $(this.el).append(button);

        if (model.expanded())
            {
              this.expButton = $(this.el).clone();
              this.expButton.addClass("select-exp").css("position","absolute");
              $('.select-button',this.expButton).addClass("select-exp").css('z-index',model.zIndex() + 2);
              this.expButton.css('z-index',model.zIndex());
              this.expButton.css('left',$(this.el).offset().left + "px").css('top',($(this.el).offset().top + "px"));
              this.expButton.css("display", "block");
              $('body').append(this.expButton);
              this.expButton.mouseout(function() {
                 setTimeout(function() {view.closeIfNeeded();}, 100);
              });
              this.expButton.mouseenter(function() {view.enterdate = new Date().getTime();});
              this.expButton.append(options.css('z-index',model.zIndex()+1));
            }
        else
            {
              if (this.expButton != undefined) $(this.expButton).detach();
            }
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
                                        textWidth : args.textWidth,
                                        expandSide : args.expandSide,
                                        onOpen : args.onOpen,
                                        extraNameAttrs: args.extraNameAttrs,
                                        expandOnHover : args.expandOnHover,
                                        offset : args.offset
                                       });
          var input = $("<div class='select'/>");
          if (args.cssClass!= undefined)
              input.addClass(args.cssClass);
          if (args.style != undefined)
              input.attr("style",args.style);

          var view = new SelectView({model : model, el : input});
          return new Object({
              model : function() {return model;},
              view : function()  {return view;},
              clear : function() {view.clear(); model.destroy();},
              open : function()  {model.expand();},
              close : function() {model.unexpand();},
              input : function() {return $(view.el);}
            });
};

})(window);
