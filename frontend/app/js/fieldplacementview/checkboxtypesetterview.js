define(['Backbone', 'legacy_code'], function(Backbone) {

window.CheckboxTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear');
        var self = this;
        self.model.bind('removed', self.clear);
        self.model.field().bind('change', self.render);
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
        this.model.field().unbind('change', this.render);
        this.model.field().signatory().unbind("change:fields",this.render);
        this.model.field().signatory().document().unbind("change:signatories",this.render);
        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);
        this.model.typeSetter = undefined;
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
            cssClass: 'signature-field-placement-setter-field-selector',
            textWidth: 191,
            optionsWidth: "218px",
            onSelect: function(s) {
                mixpanel.track('Choose checkbox signatory');
                field.signatory().deleteField(field);
                field.setSignatory(s);
                s.addField(field);
                if (s.author())
                  field.setValue("checked");
                else
                  field.setValue("");
                return true;

            }
        });

        box.text(localization.designview.textFields.forThis + " ");
        box.append($("<div style='margin-top:5px;margin-bottom:10px;'>").append(selector.el()));

        return box;
    },
    obligatoryOption : function() {
        var self = this;
        var option = $("<div class='fieldTypeSetter-option checkbox-box'/>");
        self.obligatoryCheckbox = $("<div class='checkbox'><div class='checkmark'/></div>");
        var label = $("<label/>").text(localization.designview.checkboxes.obligatory);
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
        if (field.isObligatory()) {
            mixpanel.track('Choose checkbox obligation', {
                Value: 'optional'
            });
            field.makeOptional();
        } else {
            mixpanel.track('Choose checkbox obligation', {
                Value: 'obligatory'
            });
            field.makeObligatory();
        }
        this.obligatoryCheckbox.toggleClass("checked", field.isObligatory());
    },
    precheckedOption: function() {
        var self = this;
        var option = $("<div class='fieldTypeSetter-option checkbox-box'/>");
        self.precheckedCheckbox = $("<div class='checkbox'><div class='checkmark'/></div>");
        var label = $("<label/>").text(localization.designview.checkboxes.prechecked);
        var field = self.model.field();
        option.append(self.precheckedCheckbox).append(label);
        if (field.value() != undefined && field.value()  != "") {
            self.precheckedCheckbox.addClass("checked");
        }
        self.precheckedCheckbox.click(function() {
          self.changePrecheckedOption();
        });
        label.click(function() {
          self.changePrecheckedOption();
        });

        return option;
    },
    changePrecheckedOption : function() {
        var field = this.model.field();
        var checkbox = this.precheckedCheckbox;
        if (field.value() != undefined && field.value()  != "") {
            mixpanel.track('Choose prechecked', {
                Value: 'unchecked'
            });
            checkbox.removeClass("checked");
            field.setValue("");
        } else {
            mixpanel.track('Choose prechecked', {
                Value: 'prechecked'
            });
            checkbox.addClass("checked");
            field.setValue("checked");
        }
        field.trigger("change");
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return new Button({
                            size: "tiny",
                            text: localization.designview.checkboxes.done,
                            cssClass : "fieldTypeSetter-button",
                            onClick : function() {

                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done){
                                     field.makeReady();
                                     view.clear();
                                } else {
                                    if (view.nameinput != undefined)  view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
            }).el();
    },
    title: function() {
        return $("<div class='title'/>").text(localization.designview.checkboxes.checkbox).attr("title",this.model.field().name());
    },
    subtitle : function() {
        var box = $("<div class='subtitle'/>");
        var name = this.model.field().signatory().nameInDocument();
        if (this.model.field().signatory().nameOrEmail() != "")
            name = this.model.field().signatory().nameOrEmail();
        var text = localization.designview.checkboxes.forThis + " " + name;
        box.text(text);
        return box;
    },
    place : function() {
        var placement = this.model;
        var offset = $(placement.view.el).offset();
        $(this.el).css("left",offset.left + FieldPlacementGlobal.checkboxTypeSetterHorizontalOffset);
        $(this.el).css("top",offset.top + FieldPlacementGlobal.checkboxTypeSetterVerticalOffset);
    },
    render: function() {
           var view = this;
           var container = $(this.el);
           container.empty();
           container.addClass("fieldTypeSetter-container");
           container.css("position", "absolute");
           var body = $("<div class='fieldTypeSetter-body'/>");
           var arrow = $("<div class='fieldTypeSetter-arrow'/>");
           container.append(arrow);
           container.append(body);

           body.append(this.title());
           body.append(this.selector());
           body.append(this.precheckedOption());
           body.append(this.obligatoryOption());
           body.append(this.doneOption());
           this.place();
           return this;
    }
});

});
