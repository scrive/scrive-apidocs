define(["Backbone", "React", "legacy_code"], function (Backbone, React) {

return Backbone.View.extend({
  initialize: function (args) {
    this.listenTo(this.model, "change", this.udpatePosition);
    this.listenTo(this.model.field(), "change", this.render);
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
        left: Math.round(placement.xrel() * parentWidth) - FieldPlacementGlobal.placementBorder,
        top: Math.round(placement.yrel() * parentHeight) - FieldPlacementGlobal.placementBorder,
        width: Math.round(placement.wrel() * parentWidth),
        height: Math.round(placement.hrel() * parentHeight),
        fontSize: placement.fsrel() * parentWidth
      });
    }
  },

  toggleCheck: function () {
    var field =  this.model.field();

    if (field.value() == "") {
      field.setValue("CHECKED");
    } else {
      field.setValue("");
    }

    return false;
  },

  render: function () {
    var self = this;
    var field =  self.model.field();
    var doc = field.signatory().document();
    var current = field.signatory() == doc.currentSignatory() && doc.currentSignatoryCanSign();
    var box = $("<div>");

    self.$el.addClass("placedfield");

    if (current) {
      box.addClass("placedcheckbox");
      self.$el.css("cursor", "pointer");
      self.$el.addClass("to-fill-now");
      if (field.obligatory()) {
        self.$el.addClass("obligatory");
      }

      if (!field.isClosed() && self.inlineediting != true) {
        box.click(function () {
          self.toggleCheck();
        });
      }
    } else {
      box.addClass("placedcheckbox-noactive");
    }

    self.$el.empty();

    box.toggleClass("checked", field.value() !== "");

    FieldPlacementGlobal.updateSignatoryCSSClass(box, field.signatory());

    self.$el.append(box);
  }
});

});
