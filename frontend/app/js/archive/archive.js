/* Main archive definition. Its a tab based set of different documents lists. */

define(['React', 'archive/documents','archive/templates', 'archive/attachments', 'archive/trash', 'Backbone', 'legacy_code'], function(React,DocumentsList,TemplatesList,AttachmentsList,TrashList) {

var ArchiveModel = Backbone.Model.extend({
  month : function() {
     return this.get("month");
  },
  year : function() {
     return this.get("year");
  },
  forCompanyAdmin : function() {
     return this.get("forCompanyAdmin");
  },
  forNewUser: function() {
    return this.get("forNewUser");
  },
  documentsTab : function() {
                    var archive = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(
                      new DocumentsList({
                          forCompanyAdmin : this.forCompanyAdmin(),
                          year : this.year(),
                          month : this.month(),
                          loadLater : true
                      }),div[0]);
                    return new Tab({
                        name: localization.archive.documents.name,
                        elems: [function() {return div;}],
                        pagehash : "documents",
                        onActivate : function() {
                            list.reload();
                            mixpanel.register({Subcontext : 'Documents tab'});
                            mixpanel.track('View Documents Tab');
                        }
                    });
  },
  templatesTab : function() {
                    var archive = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new TemplatesList({
                      loadLater : true
                    }),div[0]);
                    return  new Tab({
                        name: localization.archive.templates.name,
                        elems: [function() {return div;}],
                        pagehash : "templates",
                        onActivate : function() {
                            list.reload();
                            mixpanel.register({Subcontext : 'Templates tab'});
                            mixpanel.track('View Templates Tab');
                        }
                    });
  },
  attachmentsTab : function() {
                    var archive = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new AttachmentsList({
                      loadLater : true
                    }),div[0]);
                    return  new Tab({
                        name: localization.archive.attachments.name,
                        elems: [function() {return div;}],
                        pagehash : "attachments",
                        onActivate : function() {
                            list.reload();
                            mixpanel.register({Subcontext : 'Attachments tab'});
                            mixpanel.track('View Attachments Tab');
                        }
                    });
  },
  trashTab : function() {
                    var archive = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new TrashList({
                         forCompanyAdmin : this.forCompanyAdmin(),
                         year : this.year(),
                         month : this.month(),
                         loadLater : true
                    }),div[0]);
                    return  new Tab({
                        name: localization.archive.trash.name,
                        elems: [function() {return div;}],
                        pagehash : "trash",
                        onActivate : function() {
                            list.reload();
                            mixpanel.register({Subcontext : 'Trash tab'});
                            mixpanel.track('View Trash Tab');
                        }
                    });
  }
});

var ArchiveView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        var view = this;
        this.render();
        $(window).scroll(function() { view.updateScroll(); });
    },
    updateScroll: function() {
        var limit = 213;
        if ($('.blocking-info').size() > 0) limit += $('.blocking-info').outerHeight();
        if ($(window).scrollTop() >= limit && $(".wrapper-position-footer").height() >= 1200) {
            $(this.el).addClass("scrolled");
            var topbox = $(this.el).find('.option-top-box');
            var bgcolor = $('body').css('background-color');
            topbox.css('background-color', bgcolor);
            topbox.find('.subbox').css('background-color', bgcolor);
        } else if ( $(this.el).hasClass("scrolled") && $(window).scrollTop() <= limit) {
            $(this.el).removeClass("scrolled");
        }
    },
    render: function () {
       var container = $(this.el);
       var archive = this.model;
       var view = this;
       var tabs = new KontraTabs({
        tabs: [ archive.documentsTab(), archive.templatesTab(), archive.attachmentsTab(), archive.trashTab() ]
       });
       container.append(tabs.el());
       return this;
    }
});


window.Archive = function(args) {
          var model = new ArchiveModel(args);
          var view =  new ArchiveView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;}

            });
};

});
