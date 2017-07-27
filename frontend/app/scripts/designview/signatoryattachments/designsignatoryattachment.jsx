var Backbone = require("backbone");

var DesignSignatoryAttachmentModel = Backbone.Model.extend({
  defaults: {
    name: "",
    description: "",
    signatory: null,
    isRequired: true
  },
  ready: function () {
    return (
      this.get("signatory") != null &&
      this.get("name") != "" &&
      this.get("description") != ""
    );
  }
});

module.exports = DesignSignatoryAttachmentModel;
