/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window) {



var AuthorViewModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
      var self = this;
      this.document().bind('reset', function() {self.trigger("render")});
      this.document().bind('change', function() {self.trigger("render")});
  },
  document : function() {
     return this.get("document");
  },
  title : function() {
    if (this.get("title") == undefined)
      this.set({"title" : new AuthorViewTitleBox({authorview : this})}, {silent : true});
    return this.get("title");
  },
  history : function() {
    if (this.get("history") == undefined)
      this.set({"history" : new AuthorViewHistory({authorview : this})}, {silent : true});
    return this.get("history");
  },
  signatories : function() {
    if (this.get("signatories") == undefined)
      this.set({"signatories" : new AuthorViewSignatories({authorview : this})}, {silent : true});
    return this.get("signatories");
  },
  file  : function() {
    if (this.get("file") == undefined)
      this.set({"file" : KontraFile.init({
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
    return this.document().evidenceattachments().length > 0;
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
  ready : function() {
     return this.document().ready();
  }
});

  

window.AuthorViewView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('render', this.render);
    this.prerender();
    this.render();
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
    this.container.empty();
    this.container.append(this.model.title().el());
    this.container.append(this.model.history().el());
    var subcontainer = $("<div class='subcontainer'/>")
    this.container.append(subcontainer);
    subcontainer.append(this.model.signatories().el());
    subcontainer.append(model.file().view.el);
    if (this.model.hasSignatoriesAttachmentsSection())
       subcontainer.append(model.signatoryattachments().el());
    if (this.model.hasAuthorAttachmentsSection())
       subcontainer.append(model.authorattachments().el());
    if (this.model.hasEvidenceAttachmentsSection())
       subcontainer.append(model.evidenceattachments().el());
    return this;
  }
});



window.AuthorView = function(args) {
       var document = new Document({
                        id : args.id,
                        viewer: args.viewer,
                        readOnlyView: true,
                        evidenceAttachments: true
                    });
       var model = new AuthorViewModel({
                        document : document
                    });
       var view = new AuthorViewView({
                        model: model,
                        el : $("<div/>")
                    });
       document.fetch({ processData:  true, cache : false});
       this.el = function() {return $(view.el);}
}

})(window);
