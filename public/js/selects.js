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
      expanded : false
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
  expanded : function(){
       return this.get("expanded");
  },
  toggleExpand: function() {
       this.set({"expanded" : !this.expanded()});    
  }
});

/* View controls bechavior of real input vs. InfoTextInput model
 * Updates object on focus, blur and change. Sets the grey class for input and fills infotext if needed.
 */
window.SelectOptionView = Backbone.View.extend({
    events: {
        'click'  :  'selected'
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'selected');
        this.model.view = this;
        this.render();
    },
    render: function () {
        var a = $("<a/>").text(this.model.name());
        $(this.el).append(a);
        return this;
    },
    selected: function(){
        this.model.selected();
    }
    
});


var SelectView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    render: function () {
        $(this.el).empty();
        var options = $("<ul class='list-dd-opts'/>");
        var model = this.model;
        var o = this.model.options();
        _.each(model.options(),function(e){
                var li = $("<li/>");
                new SelectOptionView({model : e, el : li});
                options.append(li);
        });
        var button = $("<div class='list-dd-button'><a><p>"+model.name()+"</p></a></div>");
        button.click(function(){
            model.toggleExpand();
        });
        if (model.expanded())
            {
              button.addClass("list-dd-exp");
              options.css("display", "block");
            }
        else
            {
              button.removeClass("list-dd-exp");
              options.css("display", "none");
            }
        $(this.el).addClass("list-dd");
        $(this.el).append(button).append(options);
        return this;
    }
});



window.Select = {
    init: function (args) {
          var model = new SelectModel ({
                                        options: args.options,
                                        name : args.name,
                                        onSelect : args.onSelect
                                       });
          var input = $("<div/>");
          if (args.cssClass!= undefined)
              input.addClass(args.cssClass);
          var view = new SelectView({model : model, el : input});
          return new Object({
              input : function() {return input;}
            });
        }
    
};

})(window); 
