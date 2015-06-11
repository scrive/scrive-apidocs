define(["legacy_code", "Underscore", "Backbone", "designview/fileview/filepageview"],
  function (legacy_code, _, Backbone, FilePageView) {

var FileView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");

    this.listenTo(this.model, "reset", this.render);
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

  vline: function () {
    if (this.vlinediv != undefined) { return this.vlinediv; }
    this.vlinediv = $("<div class='vline'>");
    $(this.el).append(this.vlinediv);
    return this.vlinediv;
  },

  hline: function () {
    if (this.hlinediv != undefined) { return this.hlinediv; }
    this.hlinediv = $("<div class='hline'>");
    $(this.el).append(this.hlinediv);
    return this.hlinediv;
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

  moveCoordinateAxes: function (helper, verticaloffset, xAxisOffset, yAxisOffset) {
    var self = this;
    _.defer(function () {
      var top = helper.offset().top - $(self.el).offset().top + helper.outerHeight() + verticaloffset;
      top -= yAxisOffset || 0;
      var left = helper.offset().left - $(self.el).offset().left;
      left += xAxisOffset || 0;
      var height = $(self.el).height();
      var width = $(self.el).width();
      self.hline().css({top: top + "px", width: width + "px"});
      self.vline().css({left: left + "px", height: height + "px"});
    });
  },

  showCoordinateAxes: function (helper, verticaloffset, xAxisOffset, yAxisOffset) {
    var view = this;
    this.hline().show();
    this.vline().show();
    this.moveCoordinateAxes(helper, verticaloffset, xAxisOffset, yAxisOffset);
  },

  hideCoordinateAxes: function () {
    this.vline().hide();
    this.hline().hide();
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

return function (args) {
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

});
