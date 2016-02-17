var $ = require("jquery");
var isElementInViewport = require("../../common/iselementinviewport");
var zoomTools = require("../../common/zoomtools");
  module.exports = {
    _isInTransition: false,

    componentDidUpdate: function (prevProps, prevState) {
      if (!this._isInTransition && this.shouldTransition(prevProps, prevState)) {
        this.transition(200);
      }
    },

    scrollIntoView: function (ms) {
      var $node = $(this.getDOMNode());
      if (!isElementInViewport($node[0])) {
        var height = $node.outerHeight();
        var windowHeight = (window.innerHeight || $(window).height()) / zoomTools.zoomLevel();
        var scrollTop = $(window).scrollTop();
        var scrollBottom = scrollTop + windowHeight;
        var offsetTop = $node.offset().top;
        var offsetBottom = offsetTop + height;
        var halfGutter = 10;

        var scrollTo = 0;
        if (height > windowHeight) {
          scrollTo = offsetTop - halfGutter;
        } else {
          scrollTo = scrollTop + (offsetBottom - scrollBottom) + halfGutter;
        }

        $("html, body").animate({scrollTop: scrollTo}, ms);
      }
    },

    transition: function (ms) {
      var self = this;
      var $node = $(self.getDOMNode());
      var $firstChild = $node.children().first();
      $firstChild.css("opacity", "0");
      setTimeout(function () {
        $firstChild.css("transition", "opacity " + ms + "ms ease-in");
        $firstChild.css("opacity", "1");
        setTimeout(function () {
          $firstChild.css("opacity", "");
          $firstChild.css("transition", "");
        }, ms);
        self.scrollIntoView(ms);
      }, 1);
    }
  };
