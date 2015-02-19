define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor, Backbone) {

window.TextTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        var self = this;
        _.bindAll(this);
        self.listenTo(self.model,'removed',self.clear);
        self.listenTo(self.model,'change:field change:signatory change:step change:fsrel',self.render);

        self.listenTo(self.model.field().signatory(),'change:fields',self.render);
        self.listenTo(self.model.field().signatory().document(),"change:signatories",self.render);
        self.listenTo(self.model,'change:field',function() {
          self.listenTo(self.model.field(),'change:value',self.updatePosition);
        });
        self.listenTo(self.model.field(),'change:value',self.updatePosition);

        _.each(self.model.field().signatory().document().signatories(), function(s) {
          _.each(s.fields(), function(f) {
            _.each(f.placements(), function(p) {
              if(self.model != p && p.typeSetter) {
                p.typeSetter.clear();
              }
            });
          });
        });

        self.fixPlaceFunction = function(){
            self.place();
        };
        $(window).scroll(self.fixPlaceFunction); // To deal with resize;
        $(window).resize(self.fixPlaceFunction);
        self.render();
    },
    updatePosition : function() {
        var self = this;
        setTimeout(function() {self.place();},1);
    },
    clear: function() {
        this.off();
        this.stopListening();

        $(this.el).remove();

        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);

        this.model.typeSetter = undefined;
    },
    obligatoryOption : function() {
        var view = this;
        var field = this.model.field();
        var sig = field?field.signatory():view.model.signatory();

        var optionOptions = ['optional', 'signatory', 'sender'];

        if(sig.author())
            optionOptions = _.without(optionOptions, 'signatory');

        if(name === 'email')
            optionOptions = _.without(optionOptions, 'optional');

        if(name === 'email' && sig.needsEmail())
            optionOptions = ['sender'];

        if(name === 'mobile' && sig.needsMobile())
            optionOptions = ['sender'];

        if(name === 'sigpersnr' && sig.needsPersonalNumber())
            optionOptions = _.without(optionOptions, 'optional');

        return $("<div style='display:block;margin-bottom:5px;'/>").append(
          new TextTypeSetterOptionsView({
              model: this.model.field(),
              extraClass: 'typesetter-obligatory-option',
              options: optionOptions
          }).el);
    },
    placementOptions : function() {
        var page =  this.model.field().signatory().document().mainfile().page(this.model.get("page"));
        if (page == undefined || page.width() == undefined) return $("<div class='empty'>");

        var placement = this.model;
        var fontSizeName = localization.fontSize.custom;
        var currSize = placement.fsrel() * page.width();
        if (Math.abs(currSize - FieldPlacementGlobal.fontSizeSmall) < 1)
          fontSizeName = localization.fontSize.small;
        if (Math.abs(currSize - FieldPlacementGlobal.fontSizeNormal) < 1)
          fontSizeName = localization.fontSize.normal;
        if (Math.abs(currSize - FieldPlacementGlobal.fontSizeLarge) < 1)
          fontSizeName = localization.fontSize.big;
        if (Math.abs(currSize - FieldPlacementGlobal.fontSizeHuge) < 1)
          fontSizeName = localization.fontSize.large;
        return new Select({name : localization.fontSize.name + ": " + fontSizeName,
                           textWidth: 191,
                           optionsWidth: "218px",
                           style: "font-size: 16px;",
                           options: [
                              { name : localization.fontSize.small,
                                style: "font-size: " + FieldPlacementGlobal.fontSizeSmall + "px",
                                onSelect: function() {placement.setFSRel(FieldPlacementGlobal.fontSizeSmall/page.width()); return true;}},
                              { name : localization.fontSize.normal,
                                style: "font-size: " + FieldPlacementGlobal.fontSizeNormal + "px",
                                onSelect: function() {placement.setFSRel(FieldPlacementGlobal.fontSizeNormal/page.width()); return true;}},
                              { name : localization.fontSize.big,
                                style: "font-size: " + FieldPlacementGlobal.fontSizeLarge + "px",
                                onSelect: function() {placement.setFSRel(FieldPlacementGlobal.fontSizeLarge/page.width()); return true;}},
                              { name : localization.fontSize.large,
                                style: "font-size: " + FieldPlacementGlobal.fontSizeHuge + "px",
                                onSelect: function() {placement.setFSRel(FieldPlacementGlobal.fontSizeHuge/page.width()); return true;}}
                           ] }).el;
    },
    title : function() {
        var view = this;
        var placement = view.model;
        var field = placement.field();
        var fname = field.nicename();
        var signatory = placement.signatory();
        var sname = signatory.nameOrEmail() || signatory.nameInDocument();

        var copy = $("<div class='title'>" + localization.designview.requestFieldFrom + "</div>");
        $('.put-field-name',copy).text(fname);
        $('.put-person-name',copy).text(sname);
        return copy;
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
                                    mixpanel.track('Click save inline field');
                                    field.makeReady();
                                    view.clear();
                                    view.model.cleanTypeSetter();
                                    view.model.trigger('change:step');
                                } else {
                                    if (view.nameinput != undefined) view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
                           }).el();
    },
    place : function() {
        var placement = this.model;
        var offset = $(placement.view.el).offset();
        $(this.el).css("left",offset.left + Math.max($(placement.view.el).width() + FieldPlacementGlobal.textTypeSetterArrowOffset));
        $(this.el).css("top",offset.top - 19);
    },
    render: function() {
        var view = this;
        var container = $(this.el);

        var placement = view.model;
        var field = placement.field();
        if(placement.step() === 'edit' && field.name()) {

            container.addClass("fieldTypeSetter-container");
            container.css("position", "absolute");
            var body = $("<div class='fieldTypeSetter-body'/>");
            var arrow = $("<div class='fieldTypeSetter-arrow'/>");


            body.append(this.title());

            if (field.isAuthorUnchangeableField()) {
              var text = "";
              if (field.isEmail())
                text = localization.designview.emailCanBeChangedInAccountSection;
              else if (field.isFstName() || field.isSndName())
                text = localization.designview.nameCanBeChangedInAccountSection;
              body.append($("<div class='microcopy'/>").text(text));
            }

            if (!field.isAuthorUnchangeableField())
              body.append(this.obligatoryOption());
            body.append(this.placementOptions());


            body.append(this.doneOption());
            container.html('');
            container.append(arrow);
            container.append(body);

            this.place();
        }
        return this;
    }
});

});
