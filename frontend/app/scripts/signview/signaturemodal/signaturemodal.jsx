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
  onClose: function () {
    return this.get("onClose")();
  },
  modal: function () {
    return this.get("modal");
  },
  container: function () {
    return this.get("container");
  },
  containerTop: function () {
    return this.container().offset().top - $(window).scrollTop();
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
      return task.isSignatoryAttachmentTask();
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
    if (this.actionButtonIsSignNow()) {
      return localization.next;
    } else if (this.actionButtonIsFillInExtraDetails()) {
      return localization.pad.fillInExtraDetails;
    } else if (this.actionButtonIsApply()) {
      return localization.signature.confirmSignature;
    }
  },
  onAccept: function () {
    var self = this;
    if (this.actionButtonIsSignNow()) {
      if (!self.hasImage()) {
        self.onClose();
      } else {
        new DocumentSignConfirmation({
          model: self.signview(),
          fast: true,
          signaturesPlaced: true,
          margin: self.containerTop() + "px auto 0",
          arrow: self.arrow()
        });

        self.modal().remove();
        self.onClose();

        if (self.arrow()) {
          self.arrow().disable();
        }
      }
    } else if (this.actionButtonIsFillInExtraDetails()) {
      if (!self.hasImage()) {
        self.onClose();
      } else {
        var modal = new DocumentExtraDetailsModal({
          model: self.signview(),
          arrow:  self.arrow(),
          margin: self.containerTop() + "px auto 0",
          bottom: true
        });

        modal.popup();
        self.modal().remove();
        self.onClose();

        if (self.arrow()) {
          self.arrow().disable();
        }
      }
    } else if (this.actionButtonIsApply()) {
      self.onClose();
    }
  }
});

return function (args) {
  var self = this;
  var arrow = args.arrow();
  var width = BrowserInfo.isSmallScreen() ? 980 : 900;
  var left = Math.floor(($(window).width() - width) / 2);
  var modal = $("<div class='modal'></div>").css("height", $(document).height()).css("min-width", "1018px");
  var container = $("<div class='modal-container drawing-modal'/>").css("width", width);
  var innerHeight = 820 * args.height / args.width;

  // SalesForce1 displays our signing page in a android WebView in which $(window).height() returns 0.
  var windowHeight = $(window).height() || window.innerHeight;
  var containerTop = windowHeight - innerHeight - 240;

  container
    .css("top", $(window).scrollTop())
    .css("margin-top", containerTop)
    .css("left", "0px")
    .css("margin-left", left > 20 ? left : 20)
    .toggleClass("small-screen", BrowserInfo.isSmallScreen());

  var model = new SignatureDrawOrTypeModel({
    field: args.field,
    width: args.width,
    height: args.height,
    arrow: arrow,
    signview: args.signview,
    modal: modal,
    container: container,
    onClose: function () {
      modal.removeClass("active");
      document.ontouchmove = function (e) {
        return true;
      };
      if (arrow) {
        arrow.enable();
      }
      setTimeout(function () {modal.detach();}, 500);
    }
  });

  if (!BrowserInfo.isIE8orLower()) {
    var view  = $("<div/>");
    var processSettings = React.render(React.createElement(SignatureDrawer, {
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
    }), view[0]);
    modal.append(container.append(view));
  } else {
    var view  = $("<div/>");
    var processSettings = React.render(React.createElement(SignatureTyper, {
      field: args.field,
      width: args.width,
      height: args.height,
      acceptText: model.acceptText(),
      onClose: function () {model.onClose();},
      onAccept:  function () {model.onAccept();}
    }), view[0]);
    modal.append(container.append(view));
  }

  if (arrow) {
    arrow.disable();
  }

  $("body").append(modal);

  // If the modal (+ margin) doesn't fit when positioned at the bottom
  // position it at the top.
  if (container.height() > (window.innerHeight - 150)) {
    container.css("margin-top", 15);
  }

  modal.addClass("active");
  setTimeout(function () {
    var height = $(document).height();
    if (BrowserInfo.isIE8orLower()) {
      // WORKAROUND FOR IE8 BUG
      // overlay is absolute and has opacity filter
      // which breaks IE8 calculation of the document height
      // we temporarily disable opacity, calculate the height
      // of the overlay and use it later.
      // when we reenable opacity the document height is calculated properly
      // because of explicitly set overlay height
      var filter = modal.css("filter");
      modal.css("filter", "");
      height = $(document).height();
      modal.css("filter", filter);
    }
    modal.height(height);
  }, 600);
};

});
