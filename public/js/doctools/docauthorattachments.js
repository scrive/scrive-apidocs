/* Signatory view of document
 */


(function(window) {

var DocumentAuthorAttachmentsModel = Backbone.Model.extend({
  defaults : {
     title  : localization.authorAttachmentBoxHeader,
     textcolour : undefined,
     textfont : undefined,
     forSigning : false
  },
  initialize: function (args) {
  },
  document :function() {
     return this.get("document");
  },
  forSigning : function() {
     return this.get("forSigning");
  },
  title : function() {
     return this.get("title");
  },
  textcolour : function() {
     return this.get("textcolour");
  },
  textfont : function() {
     return this.get("textfont");
  }
});

var DocumentAuthorAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  destroy : function() {
    $(this.el).remove();
  },
  authorAttachmentDesc: function(attachment, labelCss) {
    var container = $("<div class='item' />");
    container.append($("<div class='icon' />"));
    var label = $("<div class='label' />");
    var name = $("<div class='name' />");
    name.css(labelCss);
    label.append(name.text(attachment.name()));
    container.append(label);
    container.append($("<div class='clearfix' />"));
    return container;
  },
  authorAttachmentFile: function(attachment, labelCss) {
    var model = this.model;
    var container = $("<div class='item' />");
    var buttonSize = 'small';
    var button = new Button({color: model.forSigning() ? "signview-blue" : "black", text: localization.reviewPDF, cssClass: 'float-right', size: buttonSize, onClick: function() {
                        window.open(attachment.downloadLink(), '_blank');
                        }});
    container.append(button.el());
    return container;
  },

  render: function() {
    var self = this;
    $(this.el).empty();

    if (!this.model.document().authorattachments().length > 0) {
      return this;
    }

    var container = $("<div class='authorattachments' />");

    if (BrowserInfo.isSmallScreen()) {
        container.addClass('small-screen');
    }

    var document = this.model.document();
    var labelCss = {};

    var header = $("<h2/>");
    if (this.model.textcolour()) {
      labelCss['color'] = this.model.textcolour();
    }
    if (this.model.textfont()) {
      labelCss['font-family'] = this.model.textfont();
    }
    header.css(labelCss);
    container.append(header.text(this.model.title()));

    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.document().authorattachments(), function(attachment) {
      var tr = $("<tr/>");

      tr.append($("<td class='desc'>").append(self.authorAttachmentDesc(attachment, labelCss)));
      tr.append($("<td class='file'>").append(self.authorAttachmentFile(attachment, labelCss)));
      tbody.append(tr);
    });

    container.append(table);

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
       this.el = function() {return $(view.el);};
       this.destroy = function() { view.destroy();};
};

})(window);
