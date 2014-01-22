/* Arrows
 * There are 4 types of arrows: point-left, point-right, scroll-up, scroll-down
*/

define(['Backbone', 'legacy_code'], function() {

var ArrowModel = Backbone.Model.extend({
  defaults : {
      type : undefined,
      text  : undefined,
      labelCss: undefined,
      arrowColour: undefined,
      point : undefined,
      blinks : 0 // Persistent blinking. If > 0 then arrow is in middle of blinking.
  },
  type : function(){
       return this.get("type");
  },
  blinks : function() {
      return this.get("blinks");
  },
  setBlinks : function(i) {
      return this.set({"blinks" : i});
  },
  text : function(){
       return this.get("text");
  },
  labelCss: function(){
       return this.get('labelCss');
  },
  arrowColour: function(){
       return this.get('arrowColour');
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
  },
  scrollDone : function() {
      if (this.get("scrollDone") != undefined)
        this.get("scrollDone")();
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
        _.bindAll(this, 'render','updatePosition');
        this.model.view = this;
        $(window).resize( this.updatePosition);
        this.render();
    },
    clear : function() {
        $(window).unbind('resize',this.updatePosition);
        $(this.el).remove();
    },
    fixWidth : function() {
       var container = $(this.el);
       var desiredWidth = $('.front',container).width() + $('.label',container).width() + $('.back',container).width()+10;
       if (this.right != undefined && this.right + desiredWidth > $('body').width() && BrowserInfo.isPadDevice()) {
         var maxLabelWidth = $('body').width() - this.right - $('.front',container).width() - $('.back',container).width() - 2;
         if (maxLabelWidth < 0 ) maxLabelWidth = 0;
         $('.label',container).text("").css("min-width", maxLabelWidth + "px");
         container.width($('.front',container).width() + $('.label',container).width() + $('.back',container).width()+8);
       }
       else
        container.width(desiredWidth);

    },
    updatePosition: function() {
       var container = $(this.el);
       if (this.model.point() != undefined) {
          this.right = ($(document).width() - this.model.point().offset().left );
          container.css("top", (this.model.point().offset().top + (this.model.point().outerHeight() / 2) - 19) + "px");
          container.css("right", this.right + "px");
       }
    },
    render: function () {
       var container = $(this.el);
       container.addClass('action-arrow').addClass('left');

       var front = $('<div class="front" />');
       var label = $('<div class="label" />');
       var back = $('<div class="back" />');

       if (this.model.arrowColour()) {
         BrandedImage.setBrandedImageBackground(front, 'sign-arrow-action-left-front.png', this.model.arrowColour());
         BrandedImage.setBrandedImageBackground(label, 'sign-arrow-action-label.png', this.model.arrowColour());
         BrandedImage.setBrandedImageBackground(back, 'sign-arrow-action-left-back.png', this.model.arrowColour());
       }

       container.append(front);
       container.append(label.text(this.model.text() || "" ).css(this.model.labelCss() || {}));
       container.append(back);
       this.updatePosition();
       return this;
    }
});


var PointRightArrowView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'updatePosition');
        this.model.view = this;
        $(window).resize( this.updatePosition);
        this.render();
    },
    clear : function() {
        $(window).unbind('resize',this.updatePosition);
        $(this.el).remove();
    },
    fixWidth : function() {
       var container = $(this.el);
       var desiredWidth = $('.front',container).width() + $('.label',container).width() + $('.back',container).width() + 10;
       if (this.left != undefined && this.left + desiredWidth > $('body').width() && BrowserInfo.isPadDevice()) {
         var maxLabelWidth = $('body').width() - this.left - $('.front',container).width() - $('.back',container).width() - 2;
         if (maxLabelWidth < 0 ) maxLabelWidth = 0;
         $('.label',container).text("").css("min-width", maxLabelWidth + "px");
         container.width($('.front',container).width() + $('.label',container).width() + $('.back',container).width()+10);
       }
       else
        container.width(desiredWidth);
    },
    updatePosition: function() {
       var container = $(this.el);
       if (this.model.point() != undefined) {
          var p = this.model.point();
          var top = this.model.point().offset().top;
          var height = this.model.point().outerHeight();
          if (p.children().size() == 1 && p.children().offset().top < top)
            top = p.children().offset().top;
          if (p.children().size() == 1 && p.children().outerHeight() > height)
            height = p.children().outerHeight();
          this.left = (this.model.point().offset().left + this.model.point().outerWidth() + 6);
          container.css("top", (top + (height / 2) - 19) + "px");
          container.css("left", this.left + "px");
       }
    },
    render: function () {
       var container = $(this.el);
       container.addClass('action-arrow').addClass('right');
       var front = $('<div class="front" />');
       var label = $('<div class="label" />');
       var back = $('<div class="back" />');

       if (this.model.arrowColour()) {
         BrandedImage.setBrandedImageBackground(front, 'sign-arrow-action-right-front.png', this.model.arrowColour());
         BrandedImage.setBrandedImageBackground(label, 'sign-arrow-action-label.png', this.model.arrowColour());
         BrandedImage.setBrandedImageBackground(back, 'sign-arrow-action-right-back.png', this.model.arrowColour());
       }

       container.append(front);
       container.append(label.text(this.model.text() || "" ).css(this.model.labelCss() || {}));
       container.append(back);

       this.updatePosition();
       return this;
    }
});


var ScrollUpArrowView = Backbone.View.extend({
    events: {
        "click"  :  "scroll"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'scroll', 'updateRightMargin');
        this.model.view = this;
        $(window).resize( this.updateRightMargin);
        this.render();
    },
    clear : function() {
        $(window).unbind('resize',this.updateRightMargin);
        $(this.el).remove();
    },
    updatePosition: function() {},
    render: function () {
        var view = this;
        $(this.el).addClass("up").addClass("arrow").css("cursor", "pointer");
        if (this.model.arrowColour()) {
          BrandedImage.setBrandedImageBackground(this.el, 'sign-arrow-up.png', this.model.arrowColour());
        }
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
        mixpanel.track('Click fat arrow up');
       var model = this.model;
       var task = this.model.point();
       if (task == undefined) return;
       $('html,body').animate({
          scrollTop: task.offset().top - 150
       }, 1000, function() {model.scrollDone();});
       return false;
    }

});

var ScrollDownArrowView = Backbone.View.extend({
    events: {
        "click"  :  "scroll"
    },
    clear : function() {
        $(window).unbind('resize',this.checkIfDownArrowInFooter);
        $(window).unbind('scroll',this.checkIfDownArrowInFooter);
        $(window).unbind('resize',this.updateRightMargin);
        $(this.el).remove();
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'scroll', 'checkIfDownArrowInFooter', 'updateRightMargin');
        this.model.view = this;
        $(window).resize(this.checkIfDownArrowInFooter);
        $(window).scroll(this.checkIfDownArrowInFooter);
        $(window).resize(this.updateRightMarginFunction);
        this.render();
    },
    updatePosition: function() {},
    render: function () {
        var view = this;
        $(this.el).addClass("down").addClass("arrow").css("cursor", "pointer");
        if (this.model.arrowColour()) {
          BrandedImage.setBrandedImageBackground(this.el, 'sign-arrow-down.png', this.model.arrowColour());
        }
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
        mixpanel.track('Click fat arrow down');
        var model = this.model;
        var task = this.model.point();
        if (task == undefined) return;
        var scrollbottom = task.offset().top + task.height() + 150;
        $('html,body').animate({
          scrollTop: scrollbottom - (window.innerHeight ? window.innerHeight : $(window).height())
        }, 2000, function() {model.scrollDone();});
       return false;
    }

});




window.Arrow = {
    init: function (args) {
          var model = new ArrowModel({
                          type  : args.type,
                          text  : args.text,
                          labelCss : args.labelCss,
                          arrowColour : args.arrowColour,
                          point : args.point,
                          scrollDone : args.scrollDone
                    });
          var view = new ArrowView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;},
              clear  : function() {
                  view.clear();
                  model.destroy();
              },
              updatePosition : function() {
                  view.updatePosition();
              },
              blink : function(count) {
                    var arrow = this;
                    var el = $(view.el);
                    model.setBlinks(count);
                    var startBlinking = function() {
                      var i = model.blinks();
                      if (i <= 0 ) return;
                      else if (i % 2 == 0 )
                          el.addClass('hidden');
                      else
                          el.removeClass('hidden');
                      model.setBlinks(i-1);
                      setTimeout(startBlinking,200);
                      };
                    startBlinking();

              },
              /* We need to recalculate width after appending arrow to page */
              fixWidth: function() {
                   if (view.fixWidth != undefined) view.fixWidth();
              }
            });
        }
};

});
