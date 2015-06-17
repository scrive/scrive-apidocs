define(["legacy_code", "Backbone"], function (legacy_code, Backbone) {

return Backbone.View.extend({
  initialize: function (args) {
    this.model.view = this;
    this.signview = args.signview;
    this.arrow = args.arrow;
    this.listenTo(this.model, "change", this.updatePosisiton);
    this.listenTo(this.model.field(), "change", this.render);
    this.render();
  },

  updatePosition: function () {
    var placement = this.model;
    var place = $(this.el);
    var parent = place.parent();
    if (parent.length > 0) {
      var parentWidth = parent.width();
      var parentHeight = parent.height();

      place.css({
        left: Math.round(placement.xrel() * parentWidth),
        top: Math.round(placement.yrel() * parentHeight),
        fontSize: placement.fsrel() * parentWidth
      });

      if (this.signaturePlacement !== undefined) {
        this.signaturePlacement.updateSize(placement.wrel() * parentWidth, placement.hrel() * parentHeight);
      }
    }
  },

  activateSignatureModal: function () {
    var placement = this.model;
    var field = placement.field();
    var width = placement.wrel() * this.$el.parent().width();
    var height = placement.hrel() * this.$el.parent().height();

    new SignatureDrawOrTypeModal({
      field: field,
      width: width,
      height: height,
      arrow: this.arrow,
      signview: this.signview
    });
  },

  render: function () {
    var self = this;
    var placement = self.model;
    var field = placement.field();
    var signatory = field.signatory();
    var doc = signatory.document();
    var image = field.value();
    var drawing = doc.signingInProcess() && doc.currentSignatoryCanSign() &&
      signatory.current() && !doc.readOnlyView();
    var box = $("<div>");
    var img = $("<img alt=''/>");
    var width = placement.wrel() * self.$el.parent().width();
    var height = placement.hrel() * self.$el.parent().height();

    self.$el.empty();

    self.$el.addClass("placedfield");
    self.$el.toggleClass("empty-signature", field.value() == "");

    if (drawing) {
      self.$el.css("cursor", "pointer");
      self.$el.css("z-index", "1");

      if (field.obligatory()) {
        box.addClass("obligatory");
      } else {
        box.addClass("optional");
      }

      box.addClass("signatureBox").addClass("forDrawing");
      if (image == "") {
        box.removeClass("withImage").addClass("signaturePlaceholder");
        box.width(width);
        box.height(height);
        box.css("line-height", Math.floor(height) + "px");
        box.text(localization.signature.clickToDraw);
      } else {
        box.removeClass("signaturePlaceholder").addClass("withImage");
        box.css("width", width);
        box.css("height", height);
        if (BrowserInfo.isIE7orLower() && image.substring(0, "data:".length) == "data:") {
          img.attr("src", "/img/img-signature-saved-" + localization.code + ".png");
        } else {
          img.css("width", width);
          img.attr("width", width);
          img.css("height", height);
          img.attr("height", height);
          img.attr("src", image);
        }
        box.append(img);
      }

      box.click(function () {
        self.activateSignatureModal();
      });
    } else {
      if (field.value()) {
        box.removeClass("signatureBox");
        box.css("width", width);
        box.css("height", height);

        img.attr("src", placement.field().value());
        img.css("width", width);
        img.attr("width", width);
        img.css("height", height);
        img.attr("height", height);

        box.append(img);
      }
    }

    self.$el.append(box);
  }
});

});
