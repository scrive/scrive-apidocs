var Backbone = require("backbone");
var File = require("./files.js").File;
var _ = require("underscore");

/* Signatories attachments model */


var SignatoryAttachment = exports.SignatoryAttachment = Backbone.Model.extend({
    defaults: {
        name: "",
        description: "",
        isRequired: false,
        isMarkedAsNotUploaded: false,
        loading: false,
        hasChanged: false
    },
    initialize: function(args) {
        if (args.file_id != undefined) {
          var document = args.signatory.document();
          this.set({"file": new File({
            id : args.file_id,
            name: args.file_name,
            document: document,
            documentid: document.documentid()
          })});
        }
        this.set({"isRequired": args.required});
        return this;
    },
    file: function() {
        return this.get("file");
    },
    setFile: function(file) {
        this.set({ hasChanged: true }, { silent: true });
        return this.set({'file': file});
    },
    hasChanged: function() {
      return this.get("hasChanged");
    },
    description: function() {
        return this.get("description");
    },
    name: function() {
        return this.get("name");
    },
    hasFile: function() {
        return this.file() != undefined;
    },
    signatory: function() {
        return this.get("signatory");
    },
    loading: function() {
        this.set({loading: true});
    },
    notLoading: function() {
        this.set({loading: false});
    },
    isLoading: function() {
        return this.get('loading');
    },
    isRequired: function() {
        return this.get("isRequired");
    },
    isMarkedAsNotUploaded: function() {
        return this.get("isMarkedAsNotUploaded");
    },
    setMarkedAsNotUploaded: function(marked) {
        this.set({ hasChanged: true}, {silent:true});
        this.set({"isMarkedAsNotUploaded":marked});
    },
    document: function() {
        return this.signatory().document();
    },
    draftData: function() {
        return {
              name: this.name(),
              description: this.description(),
              required: this.isRequired()
        };
    }
});
