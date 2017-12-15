var Backbone = require("backbone");
var jQuery = require("jquery");
var $ = require("jquery");
var _ = require("underscore");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;
var MailView = require("./confirmationsWithEmails.js").MailView;
var Button = require("./buttons.js").Button;

var Mail = Backbone.Model.extend({
  defaults: {
    content: jQuery("<div/>"),
    document: undefined,
    editable: false,
    editWidth: 540,
    signatory: undefined,
    ready: false
  },
  initialize: function (args) {
    var signatoryId = (
      args.signatory != undefined ? args.signatory.signatoryid() : "0"
    );

    this.url = (
      "/mailpreview/" + args.document.documentid() + "/" + signatoryId
    );
  },
  parse: function(response) {
    this.set({
      content: $("<div>").html(response.content),
      ready: true
    });
  },
  editable: function() {
    return this.get("editable");
  },
  makeEditable : function(){
    return this.set({editable : true});
  },
  content: function() {
    return this.get("content");
  },
  ready: function() {
    return this.get("ready");
  },
  editWidth: function() {
    return this.get("editWidth");
  }
});

module.exports = {
  Mail: Mail
};
