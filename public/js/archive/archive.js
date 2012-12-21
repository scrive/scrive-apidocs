/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){
 
var ArchiveModel = Backbone.Model.extend({
  month : function() {
     return this.get("month");    
  },
  year : function() {
     return this.get("year");
  },
  forCompanyAdmin : function() {
     return this.get("forCompanyAdmin");
  },
  documents : function() {
        if (this.get("documents") != undefined) return this.get("documents");
        this.set({ "documents" : new KontraList(DocumentsListDefinition(this)) });
        return this.documents();
  },
  templates : function() {
        if (this.get("templates") != undefined) return this.get("templates");
        this.set({ "templates" : new KontraList(TemplatesListDefinition(this)) });
        return this.templates();
  },
  attachments : function() {
        if (this.get("attachments") != undefined) return this.get("attachments");
        this.set({ "attachments" : new KontraList(AttachmentsListDefinition(this)) });
        return this.attachments();
  },
  bin : function() {
        if (this.get("bin") != undefined) return this.get("bin");
        this.set({ "bin" : new KontraList(BinListDefinition(this)) });
        return this.bin();

  },
  documentsTab : function() {
                    var archive = this;    
                    return new Tab({
                        name: localization.archive.documents.name,
                        elems: [function() {return $(archive.documents().el());}],
                        active : window.location.hash == "#documents",           
                        onActivate : function() {
                            window.location.hash = "documents";
                            archive.documents().recall();
                            mixpanel.register({Subcontext : 'Documents tab'});
                            mixpanel.track('View Documents Tab');
                        }
                    });
  },
  templatesTab : function() {
                    var archive = this;    
                    return  new Tab({
                        name: localization.archive.templates.name,
                        elems: [function() {return $(archive.templates().el());}],
                        active : window.location.hash == "#templates",
                        onActivate : function() {
                            window.location.hash = "templates";
                            archive.templates().recall();
                            mixpanel.register({Subcontext : 'Templates tab'});
                            mixpanel.track('View Templates Tab');
                        }
                    })
  },
  attachmentsTab : function() {
                    var archive = this;
                    return  new Tab({
                        name: localization.archive.attachments.name,
                        elems: [function() {return $(archive.attachments().el());}],
                        active : window.location.hash == "#attachments",
                        onActivate : function() {
                            window.location.hash = "attachments";
                            archive.attachments().recall();
                            mixpanel.register({Subcontext : 'Attachments tab'});
                            mixpanel.track('View Attachments Tab');
                        }
                    });
  },
  binTab : function() {
                    var archive = this;
                    return  new Tab({
                        name: localization.archive.bin.name,
                        elems: [function() {return $(archive.bin().el());}],
                        active : window.location.hash == "#bin",
                        onActivate : function() {
                            window.location.hash = "bin";
                            archive.bin().recall();
                            mixpanel.register({Subcontext : 'Bin tab'});
                            mixpanel.track('View Bin Tab');
                        }
                    });
  }
});

var ArchiveView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        var view = this;
        this.render();
        $(window).scroll(function() {view.updateScroll();})
    },
    updateScroll : function() {
        if ($(window).scrollTop() >= 226 && $("body").height() >= 1200)
        {
            $(this.el).addClass("scrolled");
        } else if ( $(this.el).hasClass("scrolled") && $(window).scrollTop() <= 226){
            $(this.el).removeClass("scrolled");
        }
            
    },
    render: function () {
       var container = $(this.el);
       var archive = this.model;
       var view = this;
       var tabs = new KontraTabs({
        tabs: [ archive.documentsTab(), archive.templatesTab(), archive.attachmentsTab(), archive.binTab() ]
       });
       container.append(tabs.view.el);
       return this;
    }
});


window.Archive = function(args) {
          var model = new ArchiveModel(args);
          var view =  new ArchiveView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;}
              
            });
};

})(window);
