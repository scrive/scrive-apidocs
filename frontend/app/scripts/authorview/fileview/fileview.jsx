var _ = require("underscore");
var Backbone = require("backbone");
var FilePageView = require("./filepageview");
var $ = require("jquery");
var File = require("../../../js/files.js").File;

var FileView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");

    this.listenTo(this.model, "change", this.render);

    this.model.view = this;
    this.pageviews = [];

    this.model.fetch({
      data: {signatoryid: this.model.signatoryid()},
      processData:  true,
      cache: false
    });

    this.render();
  },

  destroy: function () {
    this.stopListening();
    this.destroyed = true;
    _.each(this.pageviews || [], function (pv) {pv.destroy();});
    $(this.el).remove();
  },

  startReadyChecker: function () {
    if (this.destroyed) { return; }

    var view = this;

    if (view.ready()) {
      view.model.trigger("ready");
      if (view.pageviews != undefined) {
        _.each(view.pageviews, function (pv) {
          pv.updateDragablesPosition();
        });
      }
    } else {
      setTimeout(function () {view.startReadyChecker()}, 100);
    }
  },

  startReadyCheckerFirstPage: function () {
    if (this.destroyed) { return; }

    var view = this;

    if (view.readyFirstPage()) {
     view.model.trigger("FirstPageReady");
    } else {
     setTimeout(function () {view.startReadyCheckerFirstPage()}, 100);
    }
  },

  ready: function () {
    return (this.readyToConnectToPage() && $(this.el).parents("body").length > 0);
  },

  readyToConnectToPage: function () {
    return (
      this.model.ready() &&
      (this.model.pages().length > 0) &&
      (this.pageviews.length == this.model.pages().length) &&
      _.all(this.pageviews, function (pv) {return pv.ready();})
    );
  },

  readyFirstPage: function () {
    return this.pageviews.length > 0 && this.pageviews[0].ready();
  },

  render: function () {
    var view = this;
    var file = this.model;
    var docbox = $(this.el);

    docbox.empty();

    if (!file.ready()) {
      var waitbox = $("<div class='waiting4page'/>");
      docbox.append(waitbox);
    } else {
      _.each(this.pageviews, function (p) { p.destroy(); });

      this.pageviews = [];

      _.each(file.pages(), function (page) {
        var pageview = new FilePageView({
          model: page,
          arrow: view.arrow,
          signview: view.signview,
          el: $("<div/>")
        });

        view.pageviews.push(pageview);
        docbox.append($(pageview.el));
      });

      view.startReadyCheckerFirstPage();
      view.startReadyChecker();
    }
  }
});

module.exports = function (args) {
  if (args.file) {
    this.model = args.file;
  } else {
    this.model = new File({
      id: args.id,
      name: args.name,
      documentid: args.documentid,
      attachmentid: args.attachmentid,
      signatoryid: args.signatoryid
    });
  }

  this.view = new FileView({
    model: this.model,
    el: $("<div class='document-pages'/>")
  });

  this.destroy = function () {
    this.view.destroy();
  };

  this.readyToConnectToPage = function () {
    return this.view.readyToConnectToPage();
  };

  return this;
};
