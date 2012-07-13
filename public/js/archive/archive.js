/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){
 
var ArchiveModel = Backbone.Model.extend({
  documents : function() {
        if (this.get("documents") != undefined) return this.get("documents");
        this.set({ "documents" : KontraList().init(DocumentsListDefinition(this)) });
        return this.documents();
  },
  templates : function() {
        if (this.get("templates") != undefined) return this.get("templates");
        this.set({ "templates" : KontraList().init(TemplatesListDefinition(this)) });
        return this.templates();
  },
  attachments : function() {
        if (this.get("attachments") != undefined) return this.get("attachments");
        this.set({ "attachments" : KontraList().init(AttachmentsListDefinition(this)) });
        return this.attachments();
  },
  bin : function() {
        if (this.get("bin") != undefined) return this.get("bin");
        this.set({ "bin" : KontraList().init(BinListDefinition(this)) });
        return this.bin();

  },
  documentsTab : function() {
                    var archive = this;    
                    return new Tab({
                        name: localization.archive.documents.name,
                        elems: [function() {return $(archive.documents().view.el);}],
                        active : window.location.hash == "#documents",           
                        onActivate : function() {
                            window.location.hash = "documents";
                            archive.documents().recall();}
                    });
  },
  templatesTab : function() {
                    var archive = this;    
                    return  new Tab({
                        name: localization.archive.templates.name,
                        elems: [function() {return $(archive.templates().view.el);}],
                        active : window.location.hash == "#templates",
                        onActivate : function() {
                            window.location.hash = "templates";
                            archive.templates().recall();}
                    })
  },
  attachmentsTab : function() {
                    var archive = this;
                    return  new Tab({
                        name: localization.archive.attachments.name,
                        elems: [function() {return $(archive.attachments().view.el);}],
                        active : window.location.hash == "#attachments",
                        onActivate : function() {
                            window.location.hash = "attachments";
                            archive.attachments().recall();}
                    });
  },
  binTab : function() {
                    var archive = this;
                    return  new Tab({
                        name: "",
                        iconClass : "rubbishbin",
                        right: true,
                        elems: [function() {return $(archive.bin().view.el);}],
                        active : window.location.hash == "#bin",
                        onActivate : function() {
                            window.location.hash = "bin";
                            archive.bin().recall();}
                    });
  }
});

var ArchiveView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    render: function () {
       var container = $(this.el);
       var archive = this.model;
       var view = this;
       var tabs = new KontraTabs({
        title: "",
        tabs: [ archive.documentsTab(), archive.templatesTab(), archive.attachmentsTab(), archive.binTab() ]
       });
       container.append(tabs.view.el);
       return this;
    }
});


window.Archive = function(args) {
          var model = new ArchiveModel();
          var view =  new ArchiveView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;}
            });
};

})(window);
