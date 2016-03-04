var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;

/* Arrows
 * There are 4 types of arrows: point-left, point-right, scroll-up, scroll-down
*/


var pixelSpeed = 800;
var maxScrollTime = 2000;

function scrollWithInterrupt(scrollTop, timeToScroll, finish) {
  var $el = $("html,body");

  function stop() {
    $el.stop(true, false);

    $el.unbind("mousewheel", stop);
    $el.unbind("mousedown", stop);
    $el.unbind("keydown", stop);
    $el.unbind("touchstart", stop);

    finish();
  }

  $el.animate({
    scrollTop: scrollTop
  }, timeToScroll, finish);

  $el.bind("mousewheel", stop);
  $el.bind("mousedown", stop);
  $el.bind("keydown", stop);
  $el.bind("touchstart", stop);
}

var ArrowModel = Backbone.Model.extend({
  defaults : {
      type : undefined,
      margin : undefined,
      point : undefined,
      onClick : function() { return false; },
      blinks : 0 // Persistent blinking. If > 0 then arrow is in middle of blinking.
  },
  type : function(){
       return this.get("type");
  },
  blinks : function() {
      return this.get("blinks");
  },
  margin: function () {
    return this.get("margin");
  },
  setBlinks : function(i) {
      return this.set({"blinks" : i});
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
  },
  onClick : function() {
      return this.get("onClick")();
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
          this.right = ($(window).width() - this.model.point().offset().left);
          this.right += this.model.margin();
          container.css("top", (this.model.point().offset().top + (this.model.point().outerHeight() / 2) - 19) + "px");
          container.css("right", this.right + "px");
       }
    },
    render: function () {
       var container = $(this.el);
       var self = this;
       container.addClass('action-arrow').addClass('left');

       container.click(function () { self.model.onClick(); });
       var front = $('<div class="front" />');
       container.append(front);
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
          this.left = (this.model.point().offset().left + this.model.point().outerWidth() + 8);
          this.left += this.model.margin();
          container.css("top", (top + (height / 2) - 19) + "px");
          container.css("left", this.left + "px");
       }
    },
    render: function () {
       var container = $(this.el);
       var self = this;
       container.addClass('action-arrow').addClass('right');
       container.click(function () { self.model.onClick(); });
       var front = $('<div class="front" />');
       container.append(front);
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
        $(window).resize(this.updateRightMargin);
        this.render();
        this.alreadyScrolling = false;
    },
    clear : function() {
        $(window).unbind('resize',this.updateRightMargin);
        $(this.el).remove();
    },
    updatePosition: function() {},
    render: function () {
        var view = this;
        $(this.el).addClass("up").addClass("arrow").css("cursor", "pointer");
        this.updateRightMargin();
        return this;
    },
    updateRightMargin : function() {
      var ARROW_WIDTH = 102; // this must be synced with arrows.less
      var space = $(window).width() - ARROW_WIDTH;
      var arrow_margin = space / 2;
      $(this.el).css("right", arrow_margin + "px");
    },
    scroll: function(){
       if (this.alreadyScrolling) {
         return false;
       }
       this.alreadyScrolling = true;
       mixpanel.track('Click fat arrow up');
       var model = this.model;
       var task = this.model.point();
       var self = this;
       if (task == undefined) return;
       var scrollTop = task.offset().top - 150;
       var currentScrollTop = $(window).scrollTop();
       var distanceToTask = currentScrollTop - scrollTop;
       var timeToScroll = (distanceToTask / pixelSpeed) * 1000;
        if (timeToScroll > maxScrollTime) {
          timeToScroll = maxScrollTime;
        }
       scrollWithInterrupt(scrollTop, timeToScroll, function () {
         self.alreadyScrolling = false;
         model.scrollDone();
       });
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
        $(window).resize(this.updateRightMargin);
        this.render();
        this.alreadyScrolling = false;
    },
    updatePosition: function() {},
    render: function () {
        var view = this;
        $(this.el).addClass("down").addClass("arrow").css("cursor", "pointer");
        this.updateRightMargin();
        return this;
    },
    updateRightMargin : function() {
      var ARROW_WIDTH = 102; // this must be synced with arrows.less
      var space = $(window).width() - ARROW_WIDTH;
      var arrow_margin = space / 2;
      $(this.el).css("right", arrow_margin + "px");
    },
    checkIfDownArrowInFooter : function() {
      if ($(".pagefooter").size() == 0) return; //We need to do such ugly check, since footer sometimes is just hidden
      var footertop = $(".pagefooter").offset().top;
      var downarrowbottom = $(this.el).offset().top + $(this.el).height();
      if (downarrowbottom + 100 > footertop) {
        $(this.el).css("display","none");
      } else {
        $(this.el).css("display","block");
      }
    },
    scroll: function(){
        if (this.alreadyScrolling) {
          return false;
        }
        this.alreadyScrolling = true;
        mixpanel.track('Click fat arrow down');
        var self = this;
        var model = this.model;
        var task = this.model.point();

        if (task == undefined) return;

        var marginTop = 50;
        var viewportHeight = window.innerHeight ? window.innerHeight : $(window).height();
        var documentHeight = $(document).height();
        var elementWithBottom = $(document).height() - task.offset().top;
        var fitsWithBottom = elementWithBottom + marginTop < viewportHeight;

        var scrollTop = 0;
        if (fitsWithBottom) {
          scrollTop = documentHeight - viewportHeight;
        } else{
          var scrollbottom = task.offset().top + task.height() + 150;
          scrollTop = scrollbottom - viewportHeight;
        }

        var currentScrollTop = $(window).scrollTop();
        var distanceToTask = scrollTop - currentScrollTop;
        var timeToScroll = (distanceToTask / pixelSpeed) * 1000;

        if (timeToScroll > maxScrollTime) {
          timeToScroll = maxScrollTime;
        }

        scrollWithInterrupt(scrollTop, timeToScroll, function () {
          self.alreadyScrolling = false;
          model.scrollDone();
        });

       return false;
    }

});




var Arrow = exports.Arrow = function (args) {
  var model = new ArrowModel({
      type  : args.type,
      margin : args.margin,
      point : args.point,
      scrollDone : args.scrollDone,
      onClick : args.onClick
  });
  var view = new ArrowView({model : model, el : $("<div/>")});
  var blinkTimeout = undefined;
  var disabled = false;

  this.type = function() {
    return model.type();
  };
  this.el = function() {
    return $(view.el);
  };
  this.clear = function() {
    view.clear();
    model.destroy();
  };
  this.updatePosition = function() {
    view.updatePosition();
  };
  this.blink = function(count) {
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
      blinkTimeout = setTimeout(startBlinking,200);
    };

    startBlinking();
  };
  this.isBlinking = function() {
    return model.blinks() > 1;
  };
  this.isDisabled = function() {
    return disabled;
  };
  this.disable = function() {
    disabled = true;
    model.setBlinks(0);
    clearTimeout(blinkTimeout);
    $(view.el).addClass('disabled');
  };
  this.enable = function() {
    disabled = false;
    $(view.el).removeClass('disabled');

    // There is something funky going on, if I don't scroll() the arrow points
    // to the previous task, one that is already completed. There should be a better way.
    $(window).scroll();
    this.fixWidth();

    this.blink(10);
  };
  /* We need to recalculate width after appending arrow to page */
  this.fixWidth = function() {
    if (view.fixWidth != undefined) view.fixWidth();
  };
  this.activate = function() {
    if (view.scroll !== undefined) {
      view.scroll();
    } else {
      this.blink(10);
    }
  };
};

