define(["Backbone", "legacy_code"], function (Backbone) {

return Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render", "clear", "updateSignatoryCSSClass");
    this.model.bind("removed", this.clear);
    this.model.signatory().document().bind("change:signatories", this.updateSignatoryCSSClass);
    this.model.bind("change", this.render);
    this.render();
  },

  clear: function () {
    this.off();
    this.model.unbind("removed", this.clear);
    this.model.signatory().document().unbind("change:signatories", this.updateSignatoryCSSClass);
    $(this.el).remove();
  },

  updateSignatoryCSSClass: function () {
    FieldPlacementGlobal.updateSignatoryCSSClass($(this.el), this.model.signatory());
  },

  render: function () {
    var field =   this.model;
    var box = $(this.el);
    box.addClass("placedcheckbox");
    box.css("width", FieldPlacementGlobal.checkboxWidth);
    box.css("height", FieldPlacementGlobal.checkboxHeight);
    this.updateSignatoryCSSClass();
    box.toggleClass("checked", field.value() != "");
    box.toggleClass("needs-sender-action", field.needsSenderAction());

    field.bind("change", function () {
      box.toggleClass("checked", field.value() != undefined && field.value() != "");
      box.toggleClass("needs-sender-action", field.needsSenderAction());
    });
  }
});

});
