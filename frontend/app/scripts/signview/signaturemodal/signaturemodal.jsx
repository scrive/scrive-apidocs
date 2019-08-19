var Backbone = require("backbone");
var React = require("react");
var SignatureDrawer = require("./signaturedrawer");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("../../../js/utils/browserinfo.js").BrowserInfo;
var scrollToElement = require("../../common/scrolltoelement");

/* Modal for drawing or typing signature. For old IE only typing mode is available.
 * Value, as Base64 image is saved to field value.
 * Usage:
 *
 *    new SignatureDrawOrTypeModal({
 *          field: field // must be of type signature
 *          width: widthOfFinalImage
 *          height: heightOfFinalImage
 *    })
 *
 * Final image will be larger then width and height for quality reasons, but it will have same ratio.
 * Note that expected size of signatue image is not directly connected to field,
 * but rather it depends on placements and rendered page size.
 *
 */

var SignatureDrawingModel = Backbone.Model.extend({
  onClose: function () {
    return this.get("onClose")();
  },
  modal: function () {
    return this.get("modal");
  },
  field: function () {
    return this.get("field");
  },
  height: function () {
    return this.get("height");
  },
  width: function () {
    return this.get("width");
  },
  signview: function () {
    return this.get("signview");
  },
  actionButtonType: function () {
    if (this.get("actionButtonType")) {
      return this.get("actionButtonType");
    }

    var incompleteTasks = this.signview().tasks().incomplete();
    var incompleteFieldTasks = _.filter(incompleteTasks, function (task) { return task.isFieldTask(); });
    var incompleteSignatoryAttachmentsTasks = _.filter(incompleteTasks, function (task) {
      return task.isSignatoryAttachmentTask() || task.isRequiredAuthorAttachmentTask();
    });
    var numberOfIncompleteFieldTasks = incompleteFieldTasks.length;

    // Account for if this signature has already been drawn or if this signature task is the only *field* task left
    if (numberOfIncompleteFieldTasks == 1 && incompleteFieldTasks[0].field() == this.field()) {
      numberOfIncompleteFieldTasks = 0;
    }

    var fieldsLeftToFillIn = numberOfIncompleteFieldTasks > 0; // Are there more things to do on the document?

    var attachmentsLeft = incompleteSignatoryAttachmentsTasks.length > 0;

    if (attachmentsLeft || fieldsLeftToFillIn) {
      this.set({actionButtonType: "apply"}, {silent: true});
    } else if (this.signview().hasExtraInputs()) {
      this.set({actionButtonType: "extra-details"}, {silent: true});
    } else {
      // Only sign task left.
      this.set({actionButtonType: "sign-now"}, {silent: true});
    }

    return this.get("actionButtonType");
  },
  actionButtonIsSignNow: function () {
    return this.actionButtonType() == "sign-now";
  },
  actionButtonIsFillInExtraDetails: function () {
    return this.actionButtonType() == "extra-details";
  },
  actionButtonIsApply: function () {
    return this.actionButtonType() == "apply";
  },
  hasImage: function () {
    if (this.field().value()) {
      return true;
    } else {
      return false;
    }
  },
  acceptText: function () {
    return localization.next;
  },
  onAccept: function () {
    this.onClose();
  }
});

module.exports = function (args) {
  var self = this;
  var modal = $("<div class='drawer' />");
  var transTime = 300; // sync with @trans-time in 'signview/drawer.less';
  var onClose = args.onClose;
  var hideArrow = args.hideArrow;
  var showArrow = args.showArrow;
  var highlightingEnabled = args.highlightingEnabled;

  var model = new SignatureDrawingModel({
    field: args.field,
    width: args.width,
    height: args.height,
    signview: args.signview,
    modal: modal,
    onClose: function () {
      modal.removeClass("active");
      document.ontouchmove = function (e) {
        return true;
      };
      React.unmountComponentAtNode(modal[0]);
      if (onClose) {
        onClose();
      }
      if (!highlightingEnabled()) {
        showArrow();
        setTimeout(function () {
          scrollToElement(_.first(args.signview.tasks().active()).el());
        }, 5);
      }

      modal.removeClass("active");

      setTimeout(function () {
        modal.remove();
      }, transTime);
    }
  });

  React.render(React.createElement(SignatureDrawer, {
    field: args.field,
    width: args.width,
    height: args.height,
    acceptText: model.acceptText(),
    onClose: function () { model.onClose(); },
    onAccept: function () { model.onAccept(); },
    onStartDrawing: function () {
      document.ontouchmove = function (e) {
        e.preventDefault();
      };
      modal.css("-ms-touch-action", "none");
    },
    onStopDrawing: function () {
      document.ontouchmove = function (e) {
          return true;
      };
      modal.css("-ms-touch-action", "auto");
    }
  }), modal[0]);

  hideArrow();

  $(".signview").append(modal);

  // 1ms is too short for some browsers, the animation was sometimes triggered immediately.
  setTimeout(function () {
    modal.addClass("active show");
  }, 5);
};
