/* Model representing attachment during design - either with file input or with
 * attachment from attachment archive reference
 */

define(["Backbone", "legacy_code"], function (Backbone, AttachmentsListForModal) {

  return Backbone.Model.extend({
    defaults: {
      name: "",
      originaName: "",
      required: false,
      serverFileId: undefined,
      fileUpload: undefined,
      documentid: undefined
    },
    initialize: function (args) {
      this.set({originalName: args.name});
    },
    name: function () {
      return this.get("name");
    },

    setName: function (v) {
      this.set("name", v);
    },

    originalName: function () {
      return this.get("originalName");
    },

    isFile: function () {
      return this.get("fileUpload") != undefined;
    },

    isServerFile: function () {
      return this.get("serverFileId") != undefined;
    },

    fileUpload: function () {
      return this.get("fileUpload");
    },

    serverFileId: function () {
      return this.get("serverFileId");
    },

    documentid: function () {
      return this.get("documentid");
    },

    attachmentid: function () {
      return this.get("attachmentid");
    },

    downloadUrl: function () {
      var name = this.name();
      if (name.toLowerCase().indexOf(".pdf", name.length - 4) === -1) {
        name += ".pdf"; // Same old attachments have names taken directly from files and we should not add .pdf
      }
      if (this.documentid()) {
        return "/api/frontend/downloadfile/" + this.documentid() +
               "/" + this.serverFileId() + "/" + encodeURIComponent(name);
      } else if (this.attachmentid()) {
        // If documentid is not set and server file id is set - this is a attachment from attachment archive
        return "/a/download/" + this.attachmentid() + "/" + encodeURIComponent(name);
      }
    },

    isRequired: function () {
      return this.get("required");
    },

    isOptional: function () {
      return !this.get("required");
    },

    makeRequired: function () {
      this.set("required", true);
    },

    makeOptional: function () {
      this.set("required", false);
    }
  });
});
