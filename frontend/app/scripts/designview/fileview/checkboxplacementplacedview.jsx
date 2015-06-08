define(["Backbone", "React", "designview/typesetters/checkboxtypesetterview",
        "designview/fileview/checkboxplacementview", "designview/fileview/draggablefield",
        "legacy_code"],
  function (Backbone, React, CheckboxTypeSetterView, CheckboxPlacementView, draggableField) {

return Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "closeTypeSetterIfNeeded", "updatePosition", "clear", "render");

    this.model.bind("removed", this.clear);
    this.model.bind("change:xrel change:yrel change:wrel change:hrel change:fsrel", this.updatePosition, this);
    this.model.view = this;

    this.model.bind("change:withTypeSetter", this.closeTypeSetterIfNeeded);
    this.render();
  },

  closeTypeSetterIfNeeded: function () {
    if (!this.model.withTypeSetter()) {
      this.closeTypeSetter();
    }
  },

  updatePosition: function () {
    var placement = this.model;
    var place = $(this.el);
    var parent = place.parent();
    if (parent.length > 0) {
      var parentWidth = parent.width();
      var parentHeight = parent.height();
      place.css({
        left: Math.round(placement.xrel() * parentWidth) - FieldPlacementGlobal.placementBorder,
        top: Math.round(placement.yrel() * parentHeight) - FieldPlacementGlobal.placementBorder,
        width: Math.round(placement.wrel() * parentWidth),
        height: Math.round(placement.hrel() * parentHeight),
        fontSize: placement.fsrel() * parentWidth
      });
    }
  },

  clear: function () {
    this.off();
    this.model.unbind("removed", this.clear);
    this.model.unbind("change:xrel change:yrel change:wrel change:hrel change:fsrel", this.updatePosition, this);
    this.model.field().unbind("change", this.render);
    this.model.unbind("change:withTypeSetter", this.closeTypeSetterIfNeeded);

    $(this.el).remove();
  },

  hasTypeSetter: function () {
    return this.model.typeSetter != undefined;
  },

  addTypeSetter: function () {
    var placement = this.model;
    if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
      var typeSetterDiv = $("<div />");
      placement.typeSetter = React.render(React.createElement(CheckboxTypeSetterView, {
               model: placement, element: this.el
             }), typeSetterDiv[0]);
      $("body").append(typeSetterDiv);

      setTimeout(function () {
        if (placement.typeSetter) {
          placement.typeSetter.place();
        }
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
    var field =  placement.field();
    var document = field.signatory().document();
    var place = $(self.el);

    place.addClass("placedfield");
    place.css("cursor", "pointer");

    self.updatePosition();

    place.empty();

    var checkboxPlacementView = new CheckboxPlacementView({model: placement.field(), el: $("<div/>")});
    var innerPlace = $(checkboxPlacementView.el);

    place.append(innerPlace);
    self.checkboxPlacementView = checkboxPlacementView;

    draggableField(place, placement);
    innerPlace.click(function () {
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
