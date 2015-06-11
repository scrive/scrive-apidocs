define(["legacy_code", "Backbone", "React", "designview/typesetters/signaturetypesetterview",
        "designview/fileview/signatureplacementview", "designview/fileview/draggablefield"],
  function (legacy_code, Backbone, React, SignatureTypeSetterView, SignaturePlacementView, draggableField) {

return Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "updatePosition", "clear", "closeTypeSetter", "render");
    this.model.bind("removed", this.clear);
    this.model.bind("change:xrel change:yrel change:wrel change:hrel change:fsrel", this.updatePosition, this);
    this.model.bind("change:withTypeSetter", this.closeTypeSetter);
    this.model.view = this;
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

  clear: function () {
    this.off();
    $(this.el).remove();
    this.model.unbind("removed", this.clear);
    this.model.unbind("change:xrel change:yrel change:wrel change:hrel change:fsrel", this.updatePosition, this);
    this.model.unbind("change:withTypeSetter", this.closeTypeSetter);
  },

  hasTypeSetter: function () {
    return this.model.typeSetter != undefined;
  },

  addTypeSetter: function () {
    var placement = this.model;
    if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
      var typeSetterDiv = $("<div />");
      placement.typeSetter = React.render(React.createElement(SignatureTypeSetterView, {
        model: placement,
        element: this.el
      }), typeSetterDiv[0]);

      $("body").append(typeSetterDiv);

      setTimeout(function () {
        placement.typeSetter.place();
      }, 0);
    }
  },

  closeTypeSetter: function () {
    var placement = this.model;
    if (this.hasTypeSetter()) {
      placement.typeSetter.clear();
    }
  },

  render: function () {
    var self = this;

    var placement = self.model;
    var place = self.$el;
    var parent = place.parent();

    place.empty();

    place.addClass("placedfield");
    place.addClass("empty-signature");

    place.css("cursor", "pointer");
    place.css("z-index", "1");

    self.updatePosition();

    self.signaturePlacement = new SignaturePlacementView({model: placement, resizable: true});
    place.append(self.signaturePlacement.el);

    draggableField(place, placement, function () {
      return placement.wrel() * parent.width();
    }, function () {
      return placement.hrel() * parent.height();
    });

    place.click(function () {
      if (!self.hasTypeSetter()) {
        self.addTypeSetter();
      } else {
        self.closeTypeSetter();
      }

      return false;
    });

    if (placement.withTypeSetter()) {
      self.addTypeSetter();
    }
  }
});

});
