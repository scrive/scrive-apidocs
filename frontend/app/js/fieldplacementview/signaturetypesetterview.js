define(['Backbone', 'legacy_code'], function(Backbone) {

window.SignatureTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'clear', 'render');
        var self = this;
        self.model.bind('removed', self.clear);
        self.model.field().bind('change:signatory', self.render);
        self.model.field().signatory().bind("change:fields",self.render);
        self.model.field().signatory().document().bind("change:signatories",self.render);

        _.each(self.model.field().signatory().document().signatories(), function(s) {
          _.each(s.fields(), function(f) {
            _.each(f.placements(), function(p) {
              if(self.model != p && p.typeSetter) {
                p.typeSetter.clear();
              }
            });
          });
        });

        var view = self;
        self.fixPlaceFunction = function(){
            view.place();
        };
        $(window).scroll(view.fixPlaceFunction); // To deal with resize;
        $(window).resize(view.fixPlaceFunction);
        self.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        this.model.unbind('removed', this.clear);
        this.model.field().unbind('change:signatory', this.render);
        this.model.field().signatory().unbind("change:fields",this.render);
        this.model.field().signatory().document().unbind("change:signatories",this.render);
        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);
        this.model.typeSetter = undefined;
    },
    obligatoryOption : function() {
        var self = this;
        var option = $("<div class='fieldTypeSetter-option checkbox-box'/>");
        self.obligatoryCheckbox = $("<div class='checkbox'><div class='checkmark'/></div>");
        var label = $("<label/>").text(localization.designview.textFields.obligatory);
        var field = self.model.field();
        option.append(self.obligatoryCheckbox).append(label);
        if (field.isObligatory()) {
            self.obligatoryCheckbox.addClass("checked");
        }
        self.obligatoryCheckbox.click(function() {
          self.changeObligatoryOption();
        });
        label.click(function() {
          self.changeObligatoryOption();
        });

        return option;
    },
    changeObligatoryOption : function() {
        var field = this.model.field();
        var checkbox = this.obligatoryCheckbox;
        if (field.isObligatory()) {
            field.makeOptional();
        } else {
            field.makeObligatory();
        }
        checkbox.toggleClass("checked", field.isObligatory());
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return new Button({
                            size: "tiny",
                            text: localization.designview.textFields.done,
                            cssClass : "fieldTypeSetter-button",
                            onClick : function() {
                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done) {
                                    field.makeReady();
                                    view.clear();
                                } else {
                                    if (view.nameinput != undefined) view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
                           }).el();
    },
    title : function() {
        return $("<div class='title'/>").text(localization.designview.signatureBoxSettings);
    },
    selector : function() {
        var view = this;
        var box = $("<div class='subtitle'/>");
        var model = view.model;
        var field = model.field();
        var sig = field.signatory();
        var doc = sig.document();

        var signame = sig.nameOrEmail() || sig.nameInDocument();

        var options = [];

        _.each(doc.signatories(), function(s) {
            if(s !== sig)
                options.push({name: s.nameOrEmail() || s.nameInDocument(),
                              value: s});
        });

        var selector = new Select({
            name: signame,
            options: options,
            textWidth: 191,
            optionsWidth: "218px",
            cssClass: 'signature-field-placement-setter-field-selector',
            onSelect: function(s) {
                mixpanel.track('Choose signature signatory');
                field.signatory().deleteField(field);
                field.setSignatory(s);
                s.addField(field);
                return true;
            }
        });

        box.text(localization.designview.textFields.forThis + " ");
        box.append($("<div class='fieldTypeSetter-subtitle-select'>").append(selector.el()));

        return box;
    },
    place : function() {
        var placement = this.model;
        var el = $(placement.view.el);
        var offset = el.offset();
        $(this.el).css("left", offset.left + el.width() + FieldPlacementGlobal.signatureTypeSetterHorizontalOffset);
        $(this.el).css("top", offset.top + FieldPlacementGlobal.signatureTypeSetterVerticalOffset);
    },
    render: function() {
           var view = this;
           var container = $(this.el);
           container.addClass("fieldTypeSetter-container");
           container.css("position", "absolute");
           var body = $("<div class='fieldTypeSetter-body'/>");
           var arrow = $("<div class='fieldTypeSetter-arrow'/>");

           body.append(this.title());
           body.append(this.selector());
           body.append(this.obligatoryOption());

           body.append(this.doneOption());
        container.html('');
           container.append(arrow);
           container.append(body);

           this.place();
           return this;
    }
});

});
