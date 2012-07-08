/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){
 
var ArchiveModel = Backbone.Model.extend({
  defaults : {
      step : "documents"
  },
  documents : function(){
       return this.get("step") == "documents" ;
  },
  templates : function(){
       return this.get("step") == "templates";
  },
  attachments: function() {
       return this.get("step") == "attachments" ;
  },
  bin: function() {
       return this.get("step") == "bin" ;
  },
  goToDocuments : function() {
      this.set({step: "documents"});
  },
  goToTemplates : function() {
      this.set({step: "templates"});
  },
  goToAttachments : function() {
      this.set({step: "attachments"});
  },
  goToBin : function() {
      this.set({step: "bin"});
  }
});

var ArchiveView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    documents : function() {
        return $(KontraList().init(DocumentsListDefinition).view.el);

    },
    templates : function() {
        return $(KontraList().init(TemplatesListDefinition).view.el);

    },
    attachments : function() {
        return $(KontraList().init(AttachmentsListDefinition).view.el);

    },
    bin : function() {
        return $(KontraList().init(BinListDefinition).view.el);

    },
    render: function () {
       var container = $(this.el);
       var archive = this.model;
       var view = this;
       var tabs = new KontraTabs({
        title: "",
        tabs: [
            new Tab({
                active : true,
                name: localization.archive.documents.name,
                elems: [function() {return view.documents();}],
                onActivate : function() {archive.goToDocuments()}
               }),
            new Tab({
                name: localization.archive.templates.name,
                elems: [function() {return view.templates();}],
                onActivate : function() {archive.goToTemplates()}
               }),
            new Tab({
                name: localization.archive.attachments.name,
                elems: [function() {return view.attachments();}],
                onActivate : function() {archive.goToAttachments()}
               }),
            new Tab({
                name: "",
                iconClass : "rubbishbin",
                right: true,
                elems: [function() {return view.bin();}],
                onActivate : function() {archive.goToBin()}
               })
               
       ]});
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
