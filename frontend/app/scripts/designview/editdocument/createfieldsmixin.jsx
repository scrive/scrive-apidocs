var React = require("react");
var Document = require("../../../js/documents.js").Document;
var $ = require("jquery");
var _ = require("underscore");

/* Mixin for elements that can create new placements on click

   Main usage:
    self.initializeOnClickCreatePlacement({el: $("div"), ...args })

   This will create new placements on document page that is visible when div is clicked.
   Placement type and so on are provided as arguments.

 */

  module.exports = {
    propTypes: {
      model: React.PropTypes.instanceOf(Document),
      openTypeSetterFor: React.PropTypes.func.isRequired
    },

    document: function () {
      return this.props.model;
    },

    getInitialState: function () {
      return {clickedRecently: false};
    },

    // Computing position of new placement, when dragable gets clicked.
    getPositionForNewPlacementFromClick: function (x, y, expectedDimensions) {
      var self = this;
      var expectedPlacementWidth = expectedDimensions.width;
      var expectedPlacementHeight = expectedDimensions.height;
      var pageDivs = $(".pagediv");
      var hasMatch = undefined;
      var index = undefined;
      var pageDiv = undefined;

      for (var i = 0; i < pageDivs.size(); i++) {
        pageDiv = $(pageDivs[i]);
        var top = pageDiv.offset().top;
        var height = pageDiv.height();
        var plus100match  = (top < (y + 100)) && (height + top > (y + 100) + expectedPlacementHeight);
        var plus150match  = (top < (y + 150)) && (height + top > (y + 150) + expectedPlacementHeight);
        if (plus100match) {
          y += 100;
          hasMatch = true;
          index = i;
          break;
        } else if (plus150match) {
          y += 150;
          hasMatch = true;
          index = i;
          break;
        }
      }

      if (hasMatch) {
        var page = index + 1;
        var pageWidth = pageDiv.width();
        var pageHeight = pageDiv.height();
        var x = x - pageDiv.offset().left - (expectedPlacementWidth / 2);
        var y = y - pageDiv.offset().top;
        var step = 20;
        while (
             self.documentHasConflictingPlacement(page, x / pageWidth, y / pageHeight)
          && (x + step + expectedPlacementWidth <  pageWidth || y + step + expectedPlacementHeight < pageHeight)
        ) {
          if (x + step + expectedPlacementWidth <  pageWidth) {
            x += step;
          }
          if (y + step + expectedPlacementHeight < pageHeight) {
            y += step;
          }
        }
        return {
          page: page,
          x: x,
          y: y,
          pageWidth: pageWidth,
          pageHeight: pageHeight
        };
      } else {
        return false;
      }
    },

    documentHasConflictingPlacement: function (pageIndex, xrel, yrel) {
      var page = this.document().mainfile().page(pageIndex);
      return _.any(page.placements(), function (p) {
        return p.xrel() == xrel && p.yrel() == yrel;
      });
    },

    initializeOnClickCreatePlacement: function (args) {
      var self = this;
      var el = args.el;
      var isEnabledFunc = args.isEnabledFunc;
      var expectedSizeFunc = args.expectedSizeFunc;
      var newPlacementFromPosition = args.newPlacementFromPosition;

      el.click(function () {
        if (self.state.clickedRecently) {
          return;
        }
        self.setState({clickedRecently: true});
        setTimeout(function () {
          self.setState({clickedRecently: false});
        }, 200);
        if (isEnabledFunc()) {
          var x = el.offset().left + (el.width() / 2);
          var y = el.offset().top + (el.height() / 2);
          var newPosition = self.getPositionForNewPlacementFromClick(x, y, expectedSizeFunc());
          if (newPosition) {
            var newPlacement = newPlacementFromPosition(newPosition);
            self.props.openTypeSetterFor(newPlacement);
          }
        }
      });
    }
  };
