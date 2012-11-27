(function(window){

var AuthorViewSignatoriesAttachmentsModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  authorview : function() {
     return this.get("authorview");
  },
  document :function() {
     return this.authorview().document();
  }
});

var AuthorViewSignatoriesAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  createAuthorAttachmentElems: function(attachment) {
    var container = $("<div class='item' />");
    container.append($("<div class='icon' />"));
    var label = $("<div class='label' />");
    label.append($("<div class='name' />").text(attachment.name()));
    var link = $("<a target='_blank' />");
    link.text(localization.reviewPDF);
    link.attr("href", attachment.downloadLink());
    label.append(link);
    container.append(label);
    container.append($("<div class='clearfix' />"));
    return container;
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.document().authorattachments().length > 0) {
      return this;
    }

    var container = $("<div class='signatoryattachments' />");
    container.append($("<h2/>").text(localization.authorview.requestedAttachments));
    var list = $("<div class='list' />");
    var createAuthorAttachmentElems = this.createAuthorAttachmentElems;
    _.each(this.model.document().authorattachments(), function(attachment) {
      list.append(createAuthorAttachmentElems(attachment));
    });
    list.append($("<div class='clearfix' />"));
    container.append(list);

    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }

});

window.AuthorViewSignatoriesAttachments = function(args) {
          var model = new AuthorViewSignatoriesAttachmentsModel(args);
          var view =  new AuthorViewSignatoriesAttachmentsView({model : model, el :(args.el != undefined) ? args.el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);