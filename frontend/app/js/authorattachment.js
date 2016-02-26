var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");

/* Author attachments
 */

  var AuthorAttachment = exports.AuthorAttachment = Backbone.Model.extend({
      defaults: function () {
        return {
          id: 0,
          name: "",
          required: false,
          accepted: false,
          pages: undefined
        };
      },

      initialize: function (args) {
        if (args.id) {
          this.url = "/filepages/" + args.id + this.queryPart();
        }
      },

      pageUrl: function (pageNo) {
        return "/pages/" + this.fileid() + "/" + pageNo + this.queryPart();
      },

      queryPart: function () {
        var params = {
          documentid: this.documentid(),
          signatorylinkid: this.signatoryid()
        };
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

      downloadLink: function () {
        var name = this.name();
        if (name.toLowerCase().indexOf(".pdf", name.length - 4) === -1) {
          name += ".pdf"; // Same old attachments have names taken directly from files, and .pdf should not be added
        }
        return "/api/frontend/downloadfile/" + this.documentid() + "/" + this.fileid() +
               "/" + encodeURIComponent(name) + this.queryPart();
      },

      fileid: function () {
          return this.get("id");
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
        if (this.document() != undefined && this.document().viewer().signatoryid() != undefined) {
          return this.document().viewer().signatoryid();
        }
        return this.get("signatoryid");
      },

      name: function () {
          return this.get("name");
      },

      isRequired: function () {
        return this.get("required");
      },

      pages: function () {
        return this.get("pages");
      },

      isAccepted: function () {
        return this.get("accepted");
      },

      setAccepted: function (accepted) {
        this.set("accepted", accepted);
      },

      draftData: function () {
        return {id: this.fileid(), name: this.name(), required: this.isRequired()};
      },

      parse: function (response) {
          if (response.error != undefined) {
            this.set({broken: true});
          } else if (response.wait != undefined) {
            _.delay(_.bind(this.fetch, this), 2000, {
                data: {signatoryid: this.signatoryid()},
                processData:  true,
                cache: false
            });
          } else {
            this.set({pages: response.pages});
          }
      }
  });
