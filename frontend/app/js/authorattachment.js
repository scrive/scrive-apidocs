var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");

/* Author attachments
 */

  var AuthorAttachment = exports.AuthorAttachment = Backbone.Model.extend({
      defaults: function () {
        return {
          file_id: 0,
          name: "",
          required: false,
          add_to_sealed_file: true,
          accepted: false,
          pixelWidth: 1040,
          pages: undefined
        };
      },

      initialize: function (args) {
        if (args.file_id) {
          var pixelWidth = this.pixelWidth();
          this.url = "/filepages/" + args.file_id + this.queryPart({"pixelwidth": pixelWidth});
        }
      },

      pageUrl: function (pageNo) {
        var pixelWidth = this.pixelWidth();
        return "/pages/" + this.fileid() + "/" + pageNo + this.queryPart({"pixelwidth": pixelWidth});
      },

      queryPart: function (more) {
        more = more || {};
        var params = _.extend(more, {
          document_id: this.documentid(),
          signatory_id: this.signatoryid()
        });
        /*
        * Remove undefined values that may happen in the object.
        */
        _.each(_.keys(params), function (k) {
          if (params[k] === undefined) {
            delete params[k];
          }
        });

        var query = $.param(params, true);
        if (query != "") {
          return "?" + query;
        } else {
          return "";
        }
      },

      downloadLink: function (asDownload) {
        var name = this.name();
        if (name.toLowerCase().indexOf(".pdf", name.length - 4) === -1) {
          name += ".pdf"; // Same old attachments have names taken directly from files, and .pdf should not be added
        }
        return "/api/frontend/documents/" + this.documentid() + "/files/" + this.fileid() +
               "/" + encodeURIComponent(name) + this.queryPart({as_download: asDownload});
      },

      fileid: function () {
          return this.get("file_id");
      },

      document: function () {
          return this.get("document");
      },

      documentid: function () {
        if (this.document() != undefined) {
          return this.document().documentid();
        }
        return this.get("documentid");
      },

      signatoryid: function () {
        if (this.document() != undefined && this.document().currentSignatory() != undefined) {
          return this.document().currentSignatory().signatoryid();
        }
        return this.get("signatoryid");
      },

      name: function () {
          return this.get("name");
      },

      isRequired: function () {
        return this.get("required");
      },

      isAddToSealedFile: function () {
        return this.get("add_to_sealed_file");
      },

      pages: function () {
        return this.get("pages");
      },

      pixelWidth: function () {
        return this.get("pixelWidth");
      },

      isAccepted: function () {
        return this.get("accepted");
      },

      setAccepted: function (accepted) {
        this.set("accepted", accepted);
      },

      parse: function (response) {
          if (response.error != undefined) {
            this.set({broken: true});
          } else if (response.wait != undefined) {
            _.delay(_.bind(this.fetch, this), 2000, {
                data: {signatory_id: this.signatoryid()},
                processData: true,
                cache: false
            });
          } else {
            this.set({pages: response.pages});
          }
      }
  });
