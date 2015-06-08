
define(["legacy_code", "Underscore", "Backbone", "designview/fileview/signatureplacementplacedview",
        "designview/fileview/checkboxplacementplacedview", "designview/fileview/textplacementplacedview"],
  function (legacy_code, _, Backbone, SignaturePlacementPlacedView,
            CheckboxPlacementPlacedView, TextPlacementPlacedView) {

return Backbone.View.extend({
  model: FilePage,

  initialize: function (args) {
    _.bindAll(this, "render", "renderDragables", "updateDragablesPosition");
    this.listenTo(this.model, "change:dragables", this.renderDragables);
    this.render();
  },

  destroy: function () {
    _.each(this.model.placements(), function (p) {
      if (p.typeSetter) { p.typeSetter.clear(); }
    });

    this.off();
    this.stopListening();

    $(this.el).remove();
  },

  makeDropable: function () {
    var self = this;
    var page = self.model;

    $(self.el).droppable({
      drop: function (event, ui) {
        var helper = $(ui.helper);
        var top = helper.offset().top - $(self.el).offset().top - 1;
        var left = helper.offset().left - $(self.el).offset().left - 1;
        var height = $(self.el).height();
        var width = $(self.el).width();
        var onDrop = $(ui.draggable).draggable("option", "onDrop");
        onDrop(page, left, top, width, height);
        return false;
      }
    });
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

        if (field.isSignature()) {
          return new SignaturePlacementPlacedView(args);
        }

        if (field.isCheckbox()) {
          return new CheckboxPlacementPlacedView(args);
        }

        if (field.isText()) {
          return new TextPlacementPlacedView(args);
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

    this.makeDropable();
    this.renderDragables();

    return this;
  }
});

});
