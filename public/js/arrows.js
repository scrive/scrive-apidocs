/* Arrows
 * There are 4 types of arrows: point-left, point-right, scroll-up, scroll-down
*/

(function(window){

var ArrowModel = Backbone.Model.extend({
  defaults : {
      type : undefined,
      text  : undefined,
      point : undefined
  },
  type : function(){
       return this.get("type");
  },
  text : function(){
       return this.get("text");
  },
  point: function() {
       return this.get("point");
  },
  isPointLeft: function() {
      return this.type() ==  "point-left";   
  },
  isPointRight: function() {
      return this.type() ==  "point-right";
  },
  isScrollUp: function() {
      return this.type() ==  "scroll-up";
  },
  isScrollDown: function() {
      return this.type() ==  "scroll-down";
  }
});


/* General arrow view - passes control to dedicated view based on something*/
var ArrowView =  function (args) {
        if (args.model.isPointLeft())
            return new PointLeftArrowView(args);
        else if (args.model.isPointRight())
            return new PointRightArrowView(args);
        else if (args.model.isScrollUp())
            return new ScrollUpArrowView(args);
        else if (args.model.isScrollDown())
            return new ScrollDownArrowView(args);
    };


/* Views for different arrows
 */
var PointLeftArrowView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    clear : function() {
        $(this.el).remove();
    },
    fixWidth : function() {
       var container = $(this.el);
       container.width($('.front',container).width() + $('.label',container).width() + $('.back',container).width()+10);
    },
    render: function () {
       var container = $(this.el);
       container.addClass('action-arrow').addClass('left');
       container.append($("<div class='front' />"));
       container.append($("<div class='label' />").text(this.model.text() || "" ));
       container.append($("<div class='back' />"));

       if (this.model.point() != undefined) {
          container.css("top", (this.model.point().offset().top + (this.model.point().height() / 2) - 14) + "px");
          container.css("right", ($(window).width() - this.model.point().offset().left ) + "px");
       }
       return this;
    }
});


var PointRightArrowView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    clear : function() {
        $(this.el).remove();
    },
    fixWidth : function() {
       var container = $(this.el);
       container.width($('.front',container).width() + $('.label',container).width() + $('.back',container).width()+10);
    },
    render: function () {
       var container = $(this.el);
       container.addClass('action-arrow').addClass('right');
       container.append($("<div class='front' />"));
       container.append($("<div class='label' />").text(this.model.text() || "" ));
       container.append($("<div class='back' />"));

       if (this.model.point() != undefined) {
          container.css("top", (this.model.point().offset().top + (this.model.point().height() / 2) - 14) + "px");
          container.css("left", (this.model.point() .offset().left + this.model.point().width() + 10) + "px");
       }
       return this;
    }
});


var ScrollUpArrowView = Backbone.View.extend({
    events: {
        "click"  :  "scroll"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'scroll');
        this.model.view = this;
        this.render();
    },
    clear : function() {
        $(window).unbind('resize',this.updateRightMarginFunction);
        $(this.el).remove();
    },
    render: function () {
        var view = this;
        $(this.el).addClass("up").addClass("arrow").css("cursor", "pointer");
        this.updateRightMarginFunction = function() {view.updateRightMargin();};
        $(window).resize( this.updateRightMarginFunction);
        this.updateRightMargin();
        return this;
    },
    updateRightMargin : function() {
      var space = $(window).width() - 941;
      var margin = 0;
      var bigarrowmargin = 0;
      if (space > 0) {
        margin = space / 2;
        bigarrowmargin = (space + 941 - 112) / 2;
      } else {
        bigarrowmargin = (941 - 112) / 2;
      }
      $(this.el).css("right", bigarrowmargin + "px");

    },
    scroll: function(){
       var task = this.model.point();
       if (task == undefined) return;
       $('html,body').animate({
          scrollTop: task.offset().top - 150
       }, 1000);
       return false;
    }

});

var ScrollDownArrowView = Backbone.View.extend({
    events: {
        "click"  :  "scroll"
    },
    clear : function() {
        $(window).unbind('resize',this.checkIfDownArrowInFooterFunction);
        $(window).unbind('scroll',this.checkIfDownArrowInFooterFunction);
        $(window).unbind('resize',this.updateRightMarginFunction);
        $(this.el).remove();
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'scroll');
        this.model.view = this;
        this.render();
    },
    render: function () {
        var view = this;
        $(this.el).addClass("down").addClass("arrow").css("cursor", "pointer");
        this.updateRightMargin();
        this.checkIfDownArrowInFooterFunction = function() {view.checkIfDownArrowInFooter();}
        this.updateRightMarginFunction = function() {view.updateRightMargin();};
        $(window).resize(this.checkIfDownArrowInFooterFunction);
        $(window).scroll(this.checkIfDownArrowInFooterFunction);
        $(window).resize(this.updateRightMarginFunction);
        return this;
    },
    updateRightMargin : function() {
      var space = $(window).width() - 941;
      var margin = 0;
      var bigarrowmargin = 0;
      if (space > 0) {
        margin = space / 2;
        bigarrowmargin = (space + 941 - 112) / 2;
      } else {
        bigarrowmargin = (941 - 112) / 2;
      }
      $(this.el).css("right", bigarrowmargin + "px");

    },
    checkIfDownArrowInFooter : function() {
      if ($(".pagefooter").size() == 0) return;
      var footertop = $(".pagefooter").offset().top;
      var downarrowbottom = $(this.el).offset().top + $(this.el).height();
      if (downarrowbottom + 100 > footertop) {
        $(this.el).css("display","none");
      } else {
        $(this.el).css("display","block");
      }
    },
    scroll: function(){
        var task = this.model.point();
        if (task == undefined) return;
        var scrollbottom = task.offset().top + task.height() + 150;
        $('html,body').animate({
          scrollTop: scrollbottom - $(window).height()
        }, 2000);
       return false;
    }

});




window.Arrow = {
    init: function (args) {
          var model = new ArrowModel({
                          type  : args.type,
                          text  : args.text,
                          point : args.point
                    });
          var view = new ArrowView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;},
              clear  : function() {
                  view.clear();
                  model.destroy();
              },
              blink : function(i) {
                    var arrow = this;
                    var el = $(view.el);
                    if (i <= 0 ) return;
                    else if (i % 2 == 0 )
                         el.hide();
                    else
                         el.show();
                    setTimeout(function() {arrow.blink(i - 1)},200);              
              },
              /* We need to recalculate width after appending arrow to page */ 
              fixWidth: function() {
                   if (view.fixWidth != undefined) view.fixWidth();
              }
            });
        }
};

})(window);
