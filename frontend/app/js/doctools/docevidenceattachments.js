define(['Backbone', 'legacy_code'], function() {

var DocumentEvidenceAttachmentsModel = Backbone.Model.extend({
  defaults : {
     ready : false
  },
  initialize: function (args) {
    this.url = "/api/frontend/getevidenceattachments/" + args.document.documentid();
    this.fetch({cache: false});
  },
  ready: function() {
    return this.get("ready");
  },
  document: function() {
     return this.get("document");
  },
  evidenceattachments: function() {
     return this.get("evidenceattachments");
  },
  parse: function(args) {
     return {
       evidenceattachments : args.evidenceattachments,
       ready: true
     };
    }
});

var DocumentEvidenceAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    var self = this;
    this.listenTo(this.model,'change', function() {self.render();});
    this.render();
  },
  destroy : function() {
    this.stopListening();
    $(this.el).remove();
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
    var button = new Button({text: localization.reviewAttachment, cssClass: 'float-right', size:'small', onClick: function() {
                        window.open(attachment.downloadLink, '_blank');
                        }});
    container.append(button.el());
    return container;
  },

  render: function() {
    var self = this;
    $(this.el).empty();

    if (!this.model.ready() || !this.model.evidenceattachments().length > 0) {
      $(this.el).css("display","none");
      return this;
    }
    $(this.el).css("display","block");
    var container = $("<div class='s-evidenceattachments authorattachments' />");
    container.append($("<h2/>").text(localization.evidenceAttachmentBoxHeader));

    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.evidenceattachments(), function(attachment) {
      var tr = $("<tr/>");

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
                        el : args.el
                    });
       this.el = function() {return $(view.el);};
       this.destroy = function() {view.destroy();};
       this.ready = function() {return model.ready();};
};

});
