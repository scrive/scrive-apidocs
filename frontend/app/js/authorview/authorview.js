var React = require("react");
var DocumentViewSignatories = require("../../scripts/authorview/signatories/docviewsignatories");
var FileView = require("../../scripts/authorview/fileview/fileview");
var Backbone = require("backbone");
var AuthorViewHistory = require("./authorsviewhistory.js").AuthorViewHistory;
var $ = require("jquery");
var DocumentAuthorAttachments = require("../doctools/docauthorattachments.js").DocumentAuthorAttachments;
var DocumentEvidenceAttachments = require("../doctools/docevidenceattachments.js").DocumentEvidenceAttachments;
var AuthorViewSignatoriesAttachments = require("./authorviewsignatoriesattachments.js").AuthorViewSignatoriesAttachments;
var _ = require("underscore");
var Document = require("../documents.js").Document;
var AuthorViewView = require("./authorview.js").AuthorViewView;
var LoadingDialog = require("../loading.js").LoadingDialog;
var TitleBoxView = require("../../scripts/authorview/titlebox");

/* Signatory view of document
 * Now unified with author and viewer views
 */

var AuthorViewModel = Backbone.Model.extend({
  defaults : {
    dirty : false // Flag. If it is set reloads will always happend, and they will block the screen.
  },
  initialize: function (args) {
      var self = this;
      this.document().on('reset', function() {self.trigger("render")});
      this.document().on('change', function() {self.trigger("render")});
  },
  document : function() {
     return this.get("document");
  },
  title : function() {
    if (this.get("title") == undefined) {
      var div = $("<div />");
      var component = React.render(
        React.createElement(TitleBoxView, {
          authorview: this,
          document: this.document()
        }),
        div[0]
      );

      this.set({
        "title": {
          el: function () {
            return div;
          },
          component: function () {
            return component;
          },
          destroy: function() {}
        }
      });
    }

    return this.get("title");
  },
  history : function() {
    if (this.get("history") == undefined)
      this.set({"history" : new AuthorViewHistory({authorview : this})}, {silent : true});
    return this.get("history");
  },
  signatories : function() {
    var self = this;
    if (this.get("signatories") == undefined) {
       var div = $('<div/>');
       var component = React.render(React.createElement(DocumentViewSignatories,{
                           forSigning: false,
                           document : this.document(),
                           onAction: function() {self.reload(true);}
                         })
                         , div[0]);
     this.set({"signatories" :    {
         el : function() {return div;},
         component : function() {return component;}
       }
     });

    }
    return this.get("signatories");
  },
  file  : function() {
    if (this.get("file") == undefined)
      this.set({"file" : new FileView({
              file: this.document().mainfile()
            })}, {silent : true});
    return this.get("file");
  },
  hasAuthorAttachmentsSection : function() {
    return this.document().authorattachments().length > 0;
  },
  authorattachments : function() {
    if (this.get("authorattachments") == undefined)
      this.set({"authorattachments" : new DocumentAuthorAttachments({document : this.document(), el : $("<div class='section spacing'/>")})}, {silent : true});
    return this.get("authorattachments");
  },
  hasEvidenceAttachmentsSection : function() {
    return this.document().closed();
  },
  evidenceattachments : function() {
    if (this.get("evidenceattachments") == undefined)
      this.set({"evidenceattachments" : new DocumentEvidenceAttachments({document : this.document(), el : $("<div class='section spacing'/>")})}, {silent : true});
    return this.get("evidenceattachments");
  },
  hasSignatoriesAttachmentsSection : function() {
    return this.document().signatoryattachments().length > 0;
  },
  signatoryattachments : function() {
    if (this.get("signatoryattachments") == undefined)
      this.set({"signatoryattachments" : new AuthorViewSignatoriesAttachments({authorview : this,el : $("<div class='section spacing'/>")})}, {silent : true});
    return this.get("signatoryattachments");
  },
  readyToShow : function() {
    return this.document().ready() && !this.document().needRecall() && this.history().ready() && this.file().readyToConnectToPage() && (!this.hasEvidenceAttachmentsSection() || this.evidenceattachments().ready());
  },
  setDontRefresh : function() {
    this.history().setDontRefresh();
  },
  isDirty : function() {
    return this.get("dirty");
  },
  reload: function(dirty) {
    if (dirty) this.set({dirty: true});
    this.trigger("reload");
  },
  destroy: function() {
    this.document().off();
    this.title().destroy();
    if (this.get("history") != undefined)
      this.history().destroy();
    if (this.get("file") != undefined)
      this.file().destroy();
    if (this.hasAuthorAttachmentsSection())
      this.authorattachments().destroy();
    if (this.hasEvidenceAttachmentsSection())
      this.evidenceattachments().destroy();
    if (this.hasSignatoriesAttachmentsSection())
      this.signatoryattachments().destroy();
  }
});



var AuthorViewView = exports.AuthorViewView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.on('render', this.render);
    this.prerender();
    this.render();
  },
  destroy : function() {
    this.model.off();
    this.model.destroy();
    this.model.setDontRefresh();
    $(this.el).empty();
  },
  prerender: function() {
    this.container = $("<div/>");
    $(this.el).append(this.container);
    $(this.el).append("<div class='clearfix'/>");
    $(this.el).append("<div class='spacer40'/>");
  },
  render: function() {
    var model = this.model;
    var document = this.model.document();
    if (!document.ready())
        return this;
    if (document.mainfile() == undefined)
        return this;
    this.container.children().detach();
    this.container.append(this.model.title().el());
    this.container.append(this.model.history().el());
    var subcontainer = $("<div class='subcontainer'/>");
    this.container.append(subcontainer);

    var documentPagesWrapper = $("<div class='document-pages-wrapper'></div>");
    documentPagesWrapper.append(this.model.signatories().el());
    documentPagesWrapper.append(model.file().view.el);
    subcontainer.append(documentPagesWrapper);
    if (this.model.hasAuthorAttachmentsSection())
       documentPagesWrapper.append(model.authorattachments().el());
    if (this.model.hasSignatoriesAttachmentsSection())
       documentPagesWrapper.append(model.signatoryattachments().el());
    if (this.model.hasEvidenceAttachmentsSection())
       documentPagesWrapper.append(model.evidenceattachments().el());

    return this;
  }
});



var AuthorView = exports.AuthorView = function(args) {
       var version = 0;
       var self = this;
       var maindiv = $("<div/>");
       var document = new Document({
                        id : args.id,
                        viewer: args.viewer,
                        evidenceAttachments: true
                    });
       var model = new AuthorViewModel({
                        document : document
                    });
       model.on('reload', function() {self.reload()});
       var view = new AuthorViewView({
                        model: model,
                        el : maindiv
                    });
       document.fetch({ processData:  true, cache : false});
       this.el = function() {return maindiv};
       this.reload = function(force) {

                 // Bind old variables
                 var olddocument = document;
                 var oldmodel = model;
                 var oldview = view;

                 // But if current model was not ready, we wait with the reload
                 if ((!oldmodel.readyToShow() || $(".modal.active").size() > 0) && (!oldmodel.isDirty()) ) return;

                 if (oldmodel.isDirty()) LoadingDialog.open();

                 // Increase version, stop fetching some elements
                 version++;
                 var reloadversion = version;
                 oldmodel.setDontRefresh();




                 // Init new elements
                 var newdiv = $("<div/>");
                 var newdocument = new Document({
                        id : args.id,
                        viewer: args.viewer,
                        evidenceAttachments: true
                    });
                 var newmodel = new AuthorViewModel({
                                  document : newdocument
                              });
                 newmodel.on('reload', function() {self.reload()});
                 var newview = new AuthorViewView({
                                  model: newmodel,
                                  el : newdiv
                              });

                 //Replace old div with new div
                 var connectNewView = function() {
                   if (newmodel.readyToShow()) {
                     if (reloadversion == version) {

                        // Initial settings based on old model
                        newmodel.signatories().component().setCurrentIndex(oldmodel.signatories().component().currentIndex());
                        console.log("Expanded " + oldmodel.history().expanded());
                        newmodel.history().setExpanded(oldmodel.history().expanded());

                        // Connecting to page
                        maindiv.empty();
                        maindiv.replaceWith(newdiv);
                        document = newdocument;
                        model = newmodel;
                        view = newview;
                        maindiv = newdiv;
                        oldview.destroy();
                        if (oldmodel.isDirty()) LoadingDialog.close();
                     } else {
                        newview.destroy();
                    }
                   }
                   else {
                     setTimeout(connectNewView,500);
                   }
                 };
                 newdocument.on('change:ready', function() {
                    connectNewView();
                 });

                 // Start whole fetching - need to do it after we binded to change:ready
                 newdocument.fetch({ processData:  true, cache : false});


       };
};

