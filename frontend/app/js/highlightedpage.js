var Backbone = require("backbone");
var File = require("./files.js").File;
var _ = require("underscore");

var HighlightedPage = exports.HighlightedPage = Backbone.Model.extend({
    defaults: {
        page: "",
        file_id: "",
    },
    initialize: function(args) {
        if (args.file_id != undefined) {
          this.set({"file": this.newHighlightedPageFile(args.signatory.document(), args.file_id)});
        }
        return this;
    },
    page: function() {
        return this.get("page");
    },
    file: function() {
        return this.get("file");
    },
    signatory: function() {
        return this.get("signatory");
    },
    document: function() {
        return this.signatory().document();
    },
    setFile: function(file_id) {
      this.set({
        "file" : this.newHighlightedPageFile(this.document(), file_id)
      });
    },
    newHighlightedPageFile: function(document,file_id) {
      return new File({
        id : file_id,
        name: "highlighted_page_image.png",
        document: document,
        documentid: document.documentid()
      });
    }
});

