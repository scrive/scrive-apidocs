/* Signatory view of document
 */


(function(window) {

var DocumentAuthorAttachmentsModel = Backbone.Model.extend({
  defaults : {
     title  : localization.authorAttachmentBoxHeader
  },
  initialize: function (args) {
  },
  document :function() {
     return this.get("document");
  },
  title : function() {
     return this.get("title")
  }
});
  
var DocumentAuthorAttachmentsView = Backbone.View.extend({
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

    var container = $("<div class='authorattachments' />");
    container.append($("<h2/>").text(this.model.title()));
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

window.DocumentAuthorAttachments = function(args) {
       var model = new DocumentAuthorAttachmentsModel(args);
       var view = new DocumentAuthorAttachmentsView({
                        model: model,
                        el : (args.el != undefined) ? args.el : $("<div/>")
                    });
       this.el = function() {return $(view.el);}
}

})(window);
