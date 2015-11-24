/* Signatories attachments model */

define(['Backbone', 'legacy_code'], function(Backbone) {

window.SignatoryAttachment = Backbone.Model.extend({
    defaults: {
        name: "",
        description: "",
        loading: false,
        hasChanged: false
    },
    initialize: function(args) {
        if (args.file != undefined) {
            var document = args.signatory.document();
            this.set({"file": new File(_.extend(args.file, {document: document,
                                                            documentid: document.documentid()
                                                           }))});
        }
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
    document: function() {
        return this.signatory().document();
    },
    draftData: function() {
        return {
              name: this.name(),
              description: this.description()
        };
    }
});

});
