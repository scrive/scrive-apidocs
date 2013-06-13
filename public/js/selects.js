/*
  Standard select boxes used by our system.
  Usage:

   var select = new Select({
      name : "Option 1"              // Name on main button
      cssClass* : "select-1"         // Class to be added
      expandSide* : "right"          // What direction should it expand ("left" or "right")
      textWidth* : "100px"           // Unexpanded selectbox will not get much longer then given value. default "160px"
      optionsWidth* : "200px"        // How long should be an expanded area
      onOpen* : function(){}         // Function to be called when box gets opened. Usefull when wanting to get options with AJAX (ask M for example)
      color* : "red"                 // Color of text
      border* : "1px solid red"      // Border style
      style* :  "font-size :16px"     // Extra style applied to main label
      onSelect* : function(v) {}     // Function to be called when value is selected. If can be overwitten by onSelect from options. If true is returned select will be closed.
      onRemove* : function(v) {}     // If provided, remove (x) icon will be displayed over select. Functon will be executed when this icon will be clicked
      options: [
                      { name : "Option 1" // Name on option label
                        value* : "1" // Value that will be propagated to onSelect
                        onSelect* : function(v) {} // Function to be called on selection. It overwrites top level function
                        style* : "font-size: 8px" // Extra styling of label
                      },
                      { name : "Option 2"
                        value* : "2"
                        onSelect* : function(v) {}
                      },
                  ]
   });

  Interface:
      .el()          // jQuery object to be appended somewere on a page
      .open()        // Expand select box. Note that it will be automaticly closed if mouse is not over it.
      .setName(name) // Change name on button

  Details:
    - It does not use <select> tag internally.
    - On expand, clone of current el with expanded options is displayed over current position.
      This way we can have selects withing scrollable areas, that don't make this areas expand.
    - onSelect function must be provided either for whole select of for all options
    - Select box will be closed on selection only if onSelect for selected option will return true.
*/

(function( window){


// Model for individual options
var SelectOptionModel = Backbone.Model.extend({
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

// Model for whole select
var SelectModel = Backbone.Model.extend({
  defaults : {
      name  : "",
      options : [],
      expandSide : "left",
      expanded : false,
      onOpen : function() {return true;},
      color: "#364963",
      border: "1px solid #DEE4ED",
      textWidth : "160px",
      optionsWidth : "200px",
      style : "",
      cssClass : ""

  },
  initialize: function(args){
      var model = this;
      //Change static options to SelectOptionModel. Propagate onSelect function if it's not provided for option
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
  optionsWidth : function() {
       return this.get("optionsWidth");
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
  onRemove : function(){
       if (this.get("onRemove") != undefined)
        return this.get("onRemove")();
       return true;
  },
  hasRemoveOption : function(){
       return this.get("onRemove") != undefined;
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

//View for individual options
var SelectOptionView = Backbone.View.extend({
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

//View for whole select
var SelectView = Backbone.View.extend({
    initialize: function (args) {
        var self = this;
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        $(this.el).mouseout(function() {
                 setTimeout(function() {self.closeIfNeeded();}, 100);
              });
        $(this.el).mouseenter(function() {self.enterdate = new Date().getTime();});
        this.render();
    },
    //Close if we are expanded, we are not hovered and some time has passed since mouse left.
    closeIfNeeded : function() {
        if ( this.dead != true
            && this.model.expanded()
            && new Date().getTime() - this.enterdate > 50
            && (this.expButton == undefined || $(":hover", this.expButton).size() == 0)
            && (!BrowserInfo.doesNotSupportHoverPseudoclassSelector() && !BrowserInfo.isPadDevice())
           )
          this.model.toggleExpand();
    },

    // Main button - no options
    button : function() {
        var model = this.model;
        var button = $("<div class='select-button' />");
        button.append("<div class='select-button-left' />");
        button.append($("<div class='select-button-label' />")
                          .attr("style", model.style())
                          .text(model.name())
                          .css("width",model.textWidth())
                     );
        button.append("<div class='select-button-right' />");

        if (this.model.hasRemoveOption())
            button.append($("<div class='closer'/>").click(function() { model.onRemove(); model.set({"expanded" : false}); }));

        return button;
    },
    render: function () {
        var self = this;
        var model = this.model;

        //If we are rerendering we remove expanded part.
        if (this.expButton != undefined) $(this.expButton).remove();


        if (!model.expanded()) {
          // We are rerendering button if it is not expanded. We start with cleaning old buttons
          $(this.el).empty()
                    .addClass(this.model.cssClass())
                    .attr("style",this.model.style())
                    .css('color',this.model.color())
                    .css("border", this.model.border())
                    .append(this.button().click(
                        function(){
                            model.toggleExpand();
                            return false;
                        })
                    );
        }
        else
            {
           // We are rerendering button with options above current element

              // Prepare box with options
              var options = $("<ul class='select-opts'/>")
                                .addClass(this.model.expandSide())
                                .css("border", this.model.border())
                                .css("width", this.model.optionsWidth());

              _.each(model.options(),function(e){
                    options.append($(new SelectOptionView({model : e, el : $("<li/>")}).el));
              });

              // Prepare area simillar to $(this.el) with extra styles, copy of button and options appended, positioned over current el
              this.expButton = $("<div class='select select-exp'/>")
                                  .append(this.button())
                                  .append(options)
                                  .addClass(this.model.cssClass())
                                  .attr("style",this.model.style())
                                  .css('color',this.model.color())
                                  .css("border", this.model.border())
                                  .css('left',$(this.el).offset().left + "px")
                                  .css('top',$(this.el).offset().top + "px")
                                  .mouseout(function() {
                                      setTimeout(function() {self.closeIfNeeded();}, 100);
                                  })
                                  .mouseenter(function() {
                                      self.enterdate = new Date().getTime();
                                  });


              // Adding expanded button directly to page
              $('body').append(this.expButton);
            }
        return this;
    }
});


// Interface
window.Select = function(args) {
          // Build model
          var model = new SelectModel (args);

          // Build view
          var div = $("<div class='select'/>");
          var view = new SelectView({model : model, el : $("<div class='select'/>")});

          // Export interface
          this.setName = function(name) { model.setName(name);};
          this.open = function(name) { model.expand();};
          this.el = function() {return $(view.el);};

};

})(window);
