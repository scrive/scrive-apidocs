/* Modal for drawing or typing signature. For old IE only typing mode is available.
 * Value, as Base64 image is saved to field value.
 * valueTMP of field is used to store some internal values (for reediting, text mode only).
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

define(["legacy_code", "Backbone", "React",
       "signview/signaturemodal/signaturedrawer", "signview/signaturemodal/signaturetyper"],
       function (_legacy, Backbone, React, SignatureDrawer, SignatureTyper) {

var SignatureDrawOrTypeModel = Backbone.Model.extend({
  onClose: function (shouldScroll, shouldSign) {
    return this.get("onClose")(shouldScroll, shouldSign);
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
  arrow: function () {
    return this.get("arrow");
  },
  actionButtonType: function () {
    if (this.get("actionButtonType")) {
      return this.get("actionButtonType");
    }

    var incompleteTasks = this.arrow().notCompletedTasks();
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
    if (this.actionButtonIsSignNow()) {
      this.onClose(true, true);
    } else if (this.actionButtonIsFillInExtraDetails()) {
      this.onClose(true, false);
    } else if (this.actionButtonIsApply()) {
      this.onClose(false, false);
    }
  }
});

return function (args) {
  var self = this;
  var arrow = args.arrow();
  var modal = $("<div class='drawer' />");
  var transTime = 300; // sync with @trans-time in 'signview/drawer.less';
  var onClose = args.onClose;

  var model = new SignatureDrawOrTypeModel({
    field: args.field,
    width: args.width,
    height: args.height,
    arrow: arrow,
    signview: args.signview,
    modal: modal,
    onClose: function (shouldScroll, shouldSign) {
      modal.removeClass("active");
      document.ontouchmove = function (e) {
        return true;
      };

      if (onClose) {
        onClose();
      }

      if (arrow) {
        arrow.enable();
        if (shouldScroll) {
          setTimeout(function () {
            arrow.goToCurrentTask();
          }, 5);
        }
      }

      modal.removeClass("active");

      setTimeout(function () {
        modal.remove();
      }, transTime);
    }
  });

  if (!BrowserInfo.isIE8orLower()) {
    React.render(React.createElement(SignatureDrawer, {
      field: args.field,
      width: args.width,
      height: args.height,
      acceptText: model.acceptText(),
      onClose: function () {model.onClose();},
      onAccept: function () {model.onAccept();},
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
  } else {
    React.render(React.createElement(SignatureTyper, {
      field: args.field,
      width: args.width,
      height: args.height,
      acceptText: model.acceptText(),
      onClose: function () {model.onClose();},
      onAccept:  function () {model.onAccept();}
    }), modal[0]);
  }

  if (arrow) {
    arrow.disable();
  }

  var closeOverlay = function (e) {
    var onOverlay = e.target === modal[0];
    if (onOverlay) {
      model.onClose();
    }
  };

  modal.mousedown(closeOverlay);
  modal.on("touchstart", closeOverlay);

  $(".signview").append(modal);
  setTimeout(function () {
    modal.addClass("active show");
  }, 1);
};

});
