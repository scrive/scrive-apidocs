/** @jsx React.DOM */

/* Mixin for elements that can create/move placements by dragging them

   Main usage:
    self.initializeDraggable({el: $("div"), ...args })

 */

define(["React", "legacy_code"], function (React) {
  return {
    propTypes: {
      showCoordinateAxes: React.PropTypes.func.isRequired,
      hideCoordinateAxes: React.PropTypes.func.isRequired,
      moveCoordinateAxes: React.PropTypes.func.isRequired
    },

    initializeDraggable: function (args) {
      var self = this;
      var el = args.el;
      var helper = args.helper;
      var cursorAt = args.cursorAt;
      var verticalOffset = args.verticalOffset;
      var xAxisOffset = args.xAxisOffset;
      var yAxisOffset = args.yAxisOffset;
      var dropXOffset = args.dropXOffset;
      var dropYOffset = args.dropYOffset;
      var onDropOnPage = args.onDropOnPage;
      var onDropOutside = args.onDropOutside;
      var onStart = args.onStart;

      var droppedInside = false;
      var dropPage = undefined;
      var dropX = undefined;
      var dropY = undefined;
      var dropPageW = undefined;
      var dropPageH = undefined;

      el.draggable({
        appendTo: ".design-view-frame",
        scroll: false,
        cursorAt: cursorAt,
        helper: helper,
        start: function (event, ui) {
          if ($("html")[0].scrollWidth <= $(window).width()) {
            // there"s no horizontal scrollbar, so dragging away should not create one
            $("html").css("overflow-x", "hidden");
          }
          if (onStart) {
            onStart();
          }
          self.props.showCoordinateAxes(ui.helper, verticalOffset, xAxisOffset, yAxisOffset);
          droppedInside = false;
        },
        stop: function () {
          $("html").css("overflow-x", "auto");
          self.props.hideCoordinateAxes();

          if (droppedInside) {
            onDropOnPage(dropPage.number(), dropX, dropY, dropPageW, dropPageH);
            droppedInside = false;
          } else if (onDropOutside) {
            onDropOutside();
          }
        },
        drag: function (event, ui) {
          self.props.showCoordinateAxes(ui.helper, verticalOffset, xAxisOffset, yAxisOffset);
          self.props.moveCoordinateAxes(ui.helper, verticalOffset, xAxisOffset, yAxisOffset);
        },
        onDrop: function (page, x, y, w, h) {
          droppedInside = true;
          dropPage = page;
          dropX = x + dropXOffset;
          dropY = y + dropYOffset;
          dropPageW = w;
          dropPageH = h;
        }
      });
    }
  };
});
