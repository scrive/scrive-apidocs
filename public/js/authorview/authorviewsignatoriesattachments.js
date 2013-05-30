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
  attachmentDescription: function(attachment) {
    var container = $("<div class='item' />");
    var text = attachment.hasFile() ? localization.authorview.uploadedBy : localization.authorview.requestedFrom;
    var label = $("<div class='label' />");
    label.append($("<span class='name'/>").text(attachment.name())).append($("<span />").text(text)).append($("<span class='name'/>").text(attachment.signatory().nameOrEmail()));
    label.append($("<div class='description' />").text('"' + attachment.description() + '"'));
    container.append(label);
    container.append($("<div class='clearfix' />"));
    return container;
  },
  attachmentFile : function(attachment) {
    var container = $("<div class='item' />");
    //var text = attachment.hasFile() ? localization.authorview.uploadedBy : localization.authorview.requestedFrom;
    //var label = $("<div class='label' />").append($("<span />").text(text)).append($("<span class='name'/>").text(attachment.signatory().nameOrEmail()));
    //container.append(label);
    if (attachment.hasFile()) {
        var button = Button.init({color: "green", text: localization.reviewPDF, cssClass: 'float-right', size:'tiny', onClick: function() {
                        window.open(attachment.file().downloadLink(), '_blank');
                        }});
        container.append(button.input());
    }
    return container;
  },
  render: function() {
    var self = this;
    $(this.el).empty();

    if (!this.model.document().signatoryattachments().length > 0) {
      return this;
    }
    var container = $("<div class='signatoryattachments' />");
    container.append($("<h2/>").text(localization.authorview.requestedAttachments));
    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.document().signatoryattachments(), function(attachment) {
      var tr = $("<tr/>");
      
      tr.append($("<td class='desc'>").append(self.attachmentDescription(attachment)));
      tr.append($("<td class='file'>").append(self.attachmentFile(attachment)));
      tbody.append(tr);
    });
    container.append(table);

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
