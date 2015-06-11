define(["legacy_code", "Backbone"], function (legacy_code, Backbone) {

return Backbone.View.extend({
  initialize: function (args) {
    this.arrow = args.arrow;
    this.listenTo(this.model, "change", this.updatePosition);
    this.listenTo(this.model.field(), "fixWHRel", this.render);
    this.model.view = this;
    this.render();
  },

  fontSize: function () {
    var parent = $(this.el).parent();
    if (parent.length > 0) { return Math.floor(this.model.fsrel() * parent.width()); }
    return 16;
  },

  updatePosition: function () {
    var placement = this.model;
    var place = $(this.el);
    var parent = place.parent();
    if (parent.length > 0) {
      var parentWidth = parent.width();
      var parentHeight = parent.height();
      place.css({
        left: Math.floor(placement.xrel() * parentWidth + 1.5) - FieldPlacementGlobal.textPlacementXOffset,
        top: Math.floor(placement.yrel() * parentHeight + 1.5) - FieldPlacementGlobal.textPlacementYOffset,
        fontSize: Math.floor(placement.fsrel() * parentWidth)
      });
    }
  },

  startInlineEditing: function () {
    var placement = this.model;
    var field =  placement.field();
    var document = field.signatory().document();
    var place = $(this.el);
    var view = this;
    var self = this;
    if (self.inlineediting == true) {
      if (self.input != undefined) {
        var focus = $(window).scrollTop() + $(window).height() > self.input.el().offset().top
          && $(window).scrollTop() < self.input.el().offset().top;
        if (focus) {
          self.input.focus();
        }

      }
      return false;
    }

    view.inlineediting = true;
    var width = place.width() > 30 ? place.width() : 30;
    var parent = place.parent();
    if (parent.length > 0) { // Check max width so we don"t expand inline editing over page width.
      var maxWidth = (1 - placement.xrel()) * parent.width() - 36;
      if (maxWidth < width) { width = maxWidth; }
      if (width < 30) { width = 30; }

    }
    place.addClass("active");
    var accept = function () {
      view.inlineediting = false;
      place.removeClass("active");
      var val = self.input.value();
      field.setValue(val);
      field.signatory().trigger("change");
      view.render();
      field.trigger("change:inlineedited");
    };

    self.input = new InfoTextInput({
          infotext: field.nicename(),
          value: field.value(),
          cssClass: "text-inline-editing",
          autoGrowth: true,
          style: "font-size:" + this.fontSize() + "px;" +
                 "line-height: 1;" +
                 "height:" + (this.fontSize() + 4) + "px;" +
                 "border-width: 0px;" +
                 "padding:" + FieldPlacementGlobal.textPlacementSpacingString + ";",
          inputStyle: "font-size:" + this.fontSize() + "px ;" +
                       "line-height: " + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px;" +
                       "height:" + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px;" +
                       "width: " + width + "px;" +
                       "background:transparent;",
          okStyle:  "font-size:" + this.fontSize() + "px ;" +
                     "line-height: " + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px;" +
                     "height:" + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight / 2) + "px;",
          onEnter: accept,
          onAutoGrowth: function () {
            if (self.arrow()) {
              self.arrow().updatePosition();
            }
          },
          onTab: accept,
          onBlur: accept,
          onOk: accept
        });
    place.empty().css("z-index", "1").append(self.input.el());
    self.input.render();
    field.trigger("change:inlineedited");
    field.bind("change", function () { view.inlineediting  = false;place.removeClass("active"); view.render();});

    var focus = $(window).scrollTop() + $(window).height() > self.input.el().offset().top &&
      $(window).scrollTop() < self.input.el().offset().top;

    if (focus) {
      self.input.focus();
    }
    return false;
  },

  fixWHRel: function () {
    var check = $(this.el).width() && $(this.el).width() > 0 &&
      $(this.el).height() && $(this.el).height() > 0;
    if (check) {
      this.model.fixWHRel($(this.el).width(), $(this.el).height());
    }
  },

  render: function () {
    var self = this;
    var field = self.model.field();
    var signatory = self.model.signatory() || field.signatory();
    var doc = signatory.document();
    var current = signatory == doc.currentSignatory() && doc.currentSignatoryCanSign();
    var fontSize = this.fontSize();
    var wrapper = $("<div class='placedfield-placement-wrapper'>");
    var box = $("<div>");

    self.$el.empty();

    self.$el.addClass("placedfield");

    if (current) {
      self.$el.css("cursor", "pointer");
    }

    box.addClass("placedfieldvalue value");
    box.css("padding", FieldPlacementGlobal.textPlacementSpacingString);

    if (field) {
      box.text(field.nicetext());
      field.bind("change", function () {
        box.text(field.nicetext());
      });
    } else {
      box.text("unset field");
    }

    if (fontSize) {
      box.css("font-size", fontSize + "px");
      box.css("line-height", fontSize +
        FieldPlacementGlobal.textPlacementExtraLineHeight + "px");
    }

    wrapper.append(box);
    self.$el.append(wrapper);

    self.$el.unbind("click");

    var canSign = field && signatory.canSign() && !field.isClosed() &&
      field.signatory().current() && self.inlineediting != true &&
      !doc.readOnlyView();

    if (canSign) {
      self.$el.addClass("to-fill-now");
      if (field.obligatory()) {
        self.$el.addClass("obligatory");
      }

      self.$el.toggleClass("empty-text-field", field.value() == "");

      self.$el.click(function () {
        return self.startInlineEditing();
      });
    } else if (!field.value()) {
      self.$el.css("display", "none");
    }
  }
});

});
