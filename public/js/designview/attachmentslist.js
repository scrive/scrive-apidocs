/* This is component for designing signatory attachments
 */

(function(window){

var DesignAttachmentsListView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.document().bind('change:attachments', this.render);
        this.model.document().bind('change:signatories', this.render);
        this.model.bind('change:step', this.render);
        this.model = args.model;
        this.render();
    },
    destroy : function() {
        this.model.document().unbind('change:attachments', this.render);
        this.model.document().unbind('change:signatories', this.render);
        this.model.unbind('change:step', this.render);

        this.off();
        this.remove();
    },
    aarow : function(a) {
      var self = this;
      var tr = $("<tr/>");
      var icon = $("<td class='icon-td'/>").append("<div class='author-attachment-icon'>");
      var name = $("<td class='name-td'/>").text(a.name() + " " + localization.designview.attached);
      var remove = $("<td class='remove-td'/>")
            .append($("<div class='remove-icon'/>")
                    .click(function() {
                        self.model.document().removeattachment(a);
                        mixpanel.track('Click remove attachment', {
                            Type: 'Author'
                        });
                    }));
      return tr.append(icon).append(name).append(remove);
    },
    sarow : function(sig,a) {
      var text = sig.nameOrEmail();
      if (sig.isCsv())
        text = localization.csv.title;
      if (text == "")
        text = sig.nameInDocument();
      var icon = $("<td class='icon-td'/>").append("<div class='signatory-attachment-icon'>");
      var tr = $("<tr/>");
      var name = $("<td class='name-td'/>").text(a.name() + " "+localization.designview.requestedFrom+" " + text.trim() + ".");
      var remove = $("<td class='remove-td'/>")
            .append($("<div class='remove-icon'/>")
                    .click(function() {
                        sig.removeAttachment(a);
                        mixpanel.track('Click remove attachment', {
                            Type: 'Signatory'
                        });
                    }));
      return tr.append(icon).append(name).append(remove);
    },
    render: function () {
        console.log("Rendering attachments list");
        var view = this;
        var document = this.model.document();
        this.container = $(this.el);
        this.container.empty();
        var authorattachments = document.authorattachments();
        var sattachments = _.flatten(_.map(document.signatories(),function(s) {return s.attachments()}));
        if (authorattachments.length != 0 || sattachments.length != 0 )
        {
            var table= $("<table/>");
            var th1 = $("<th class='icon-td'>");
            var th2 = $("<th class='name-td'>");
            var th3 = $("<th class='remove-td'>");
            var thead = $("<thead/>").append($("<tr/>").append(th1).append(th2).append(th3));
            var tbody = $("<tbody/>");
            _.each(authorattachments, function(a) { tbody.append(view.aarow(a));});
            _.each(document.signatories(),function(s) {_.each(s.attachments(),function(a) { tbody.append(view.sarow(s,a));}); });
            this.container.append(table.append(thead).append(tbody));        }
        return this;
    }
});


window.DesignAttachmentsList = function(args) {
    var view = new DesignAttachmentsListView({model : args.viewmodel, el : $("<div class=designview-attachemnts-list/>")});
    this.el = function() {return $(view.el);};
    this.destroy = function() { view.destroy();}

};


})(window);
