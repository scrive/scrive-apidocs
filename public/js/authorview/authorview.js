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
      this.set({"title" : new AuthorViewHistory({})}, {silent : true});
    return this.get("title");
  },
  history : function() {
    if (this.get("history") == undefined)
      this.set({"history" : new AuthorViewHistory({authorview : this})}, {silent : true});
    return this.get("history");
  },
  signatories : function() {
    if (this.get("signatories") == undefined)
      this.set({"signatories" : new AuthorViewSignatories({})}, {silent : true});
    return this.get("signatories");
  },
  file  : function() {
    if (this.get("file") == undefined)
      this.set({"file" : KontraFile.init({
              file: this.document().mainfile()
            })}, {silent : true});
    return this.get("file");
  },
  authorattachment : function() {
    if (this.get("authorattachment") == undefined)
      this.set({"authorattachment" : new AuthorViewAuthorAttachments({})}, {silent : true});
    return this.get("authorattachment");
  },
  signatoryattachments : function() {
    if (this.get("signatoryattachments") == undefined)
      this.set({"signatoryattachments" : new AuthorViewSiegnatoriesAttachments({})}, {silent : true});
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
    this.container = $("<div class='mainContainer' />");
    $(this.el).addClass("body-container");
    $(this.el).append(this.container);
    $(this.el).append("<div class='clearfix'/>");
    $(this.el).append("<div class='spacer40'/>");
  },
  render: function() {
    var model = this.model;
    var document = this.model.document();
    if (!document.ready())
        return this;

    this.container.empty();
    this.container.append(BlockingInfo.el());
    this.container.append(this.model.history().el());
    var subcontainer = $("<div class='subcontainer'/>")
    this.container.append(subcontainer);
    subcontainer.append(model.file().view.el);
    return this;
  }
});



window.AuthorView = function(args) {
       var document = new Document({
                        id : args.id,
                        viewer: args.viewer
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
