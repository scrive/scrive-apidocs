(function(window) {

var DocumentEvidenceAttachmentsModel = Backbone.Model.extend({
  defaults : {
     title  : localization.evidenceAttachmentBoxHeader
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
  
var DocumentEvidenceAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  evidenceAttachmentDesc: function(attachment) {
    var container = $("<div class='item' />");
    container.append($("<div class='icon' />"));
    var label = $("<div class='label' />");
    label.append($("<div class='name' />").text(attachment.name));
    container.append(label);
    container.append($("<div class='clearfix' />"));
    return container;
  },
  evidenceAttachmentFile: function(attachment) {
    var container = $("<div class='item' />");
    var button = Button.init({color: "green", text: localization.reviewAttachment, cssClass: 'float-right', size:'tiny', onClick: function() {
                        window.open(attachment.downloadLink, '_blank');
                        }});
    container.append(button.input());
    return container;
  },
  
  render: function() {
    var self = this;
    $(this.el).empty();

    if (!this.model.document().evidenceattachments().length > 0) {
      return this;
    }

    var container = $("<div class='authorattachments' />");
    container.append($("<h2/>").text(this.model.title()));

    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.document().evidenceattachments(), function(attachment) {
      var tr = $("<tr/>")

      tr.append($("<td class='desc'>").append(self.evidenceAttachmentDesc(attachment)));
      tr.append($("<td class='file'>").append(self.evidenceAttachmentFile(attachment)));
      tbody.append(tr);
    });

    container.append(table);

    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

window.DocumentEvidenceAttachments = function(args) {
       var model = new DocumentEvidenceAttachmentsModel(args);
       var view = new DocumentEvidenceAttachmentsView({
                        model: model,
                        el : (args.el != undefined) ? args.el : $("<div/>")
                    });
       this.el = function() {return $(view.el);}
}

})(window);
