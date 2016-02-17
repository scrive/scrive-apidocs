var _ = require("underscore");
var Backbone = require("backbone");
var SignaturePlacementPlacedView = require("./signatureplacementplacedview");
var CheckboxPlacementPlacedView = require("./checkboxplacementplacedview");
var TextPlacementPlacedView = require("./textplacementplacedview");
var FilePage = require("../../../js/files").FilePage;
var $ = require("jquery");

module.exports = Backbone.View.extend({
  model: FilePage,

  initialize: function (args) {
    _.bindAll(this, "render", "renderDragables", "updateDragablesPosition");
    this.listenTo(this.model, "change:dragables", this.renderDragables);
    this.render();
  },

  destroy: function () {
    this.off();
    this.stopListening();
    $(this.el).remove();
  },

  renderDragables: function () {
    var view = this;
    var page = this.model;
    var container = $(this.el);
    var file = page.file();
    _.each(page.placements(), function (placement) {
      var placement = placement;
      if (!placement.placed() && placement.page() == page.number()) {
        var elem = $("<div />").appendTo(container);
        var field = placement.field();
        var args = {
          model: placement,
          el: elem
        };

        var view = undefined;

        if (field.isSignature()) {
          view = new SignaturePlacementPlacedView(args);
        }

        if (field.isCheckbox()) {
          view = new CheckboxPlacementPlacedView(args);
        }

        if (field.isText()) {
          view = new TextPlacementPlacedView(args);
        }

        if (view !== undefined) {
          placement.view = view;
          return view;
        }

        throw new Error("unknown field type");
      }
    });
  },

  updateDragablesPosition: function () {
    var page = this.model;
    _.each(page.placements(), function (placement) {
      if (placement.placed() && placement.page() == page.number() && placement.view && placement.view.updatePosition) {
        placement.view.updatePosition();
      }
    });
  },

  ready: function () {
    var ready = this.pagejpg && this.pagejpg[0].complete;

    if (ready) {
      this.model.setSize(this.pagejpg.width(), this.pagejpg.height());
    }

    return ready;
  },

  render: function () {
    var page = this.model;
    var file = page.file();
    var fileid = file.fileid();
    var container = $(this.el);

    container.empty();
    container.attr("id", "page" + page.number());
    container.addClass("pagediv");

    this.pagejpg = $("<img class='pagejpg'/>");
    var pagelink = "/pages/" + fileid  + "/" + page.number() + file.queryPart({"pixelwidth": page.width()});

    this.pagejpg.attr("src", pagelink);
    container.append(this.pagejpg);

    this.renderDragables();

    return this;
  }
});
