define(['Backbone', 'legacy_code'], function(Backbone) {

window.SignaturePlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear', 'updateSignatoryCSSClass');
        this.model.bind('removed', this.clear);
        if (this.model.field().signatory().fstnameField() != undefined)
          this.model.field().signatory().fstnameField().bind('change', this.render);
        if (this.model.field().signatory().sndnameField() != undefined)
            this.model.field().signatory().sndnameField().bind('change', this.render);
        this.model.bind('change', this.render);
        this.model.field().bind('change:signatory', this.render);
        this.model.field().signatory().document().bind('change:signatories', this.updateSignatoryCSSClass);
        this.resizable = args.resizable;
        this.render();
    },
    clear: function() {
        this.off();
        this.model.field().unbind('change:signatory', this.render);
        this.model.unbind('change', this.render);
        this.model.unbind('removed', this.clear);
        this.model.field().signatory().document().unbind('change:signatories', this.updateSignatoryCSSClass);

        if (this.model.field().signatory().fstnameField() != undefined)
          this.model.field().signatory().fstnameField().unbind('change', this.render);
        if (this.model.field().signatory().sndnameField() != undefined)
            this.model.field().signatory().sndnameField().unbind('change', this.render);
        $(this.el).remove();
    },
    ddIcon : function() {
        return $("<div class='signatureDDIcon'/>");
    },
    header : function() {
        var placement = this.model;
        var signatory = this.model.field().signatory();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  localization.process.signatoryname + " " + signatory.signIndex();
        }

        if (placement.field().value() == "") {
            var text = $("<span>" + localization.signature.placeFor + "</span>");
            text.find(".put-name-here").text(sname);
            box.append(text);
        }

        return box;
    },
    updateSignatoryCSSClass : function() {
      FieldPlacementGlobal.updateSignatoryCSSClass($(this.el),this.model.field().signatory());
    },
    updateSize: function(width, height) {
      var box = $(this.el);
      box.width(width);
      box.height(height);
    },
    render: function() {
            var placement = this.model;
            var view = this;
            var signatory = this.model.field().signatory();
            var box = $(this.el);
            box.empty();
            var width =  placement.wrel() * box.parent().parent().width();
            var height = placement.hrel() * box.parent().parent().height();
            if (placement.field().value() == "")
            {
                box.addClass('signatureBox');
                box.append(this.ddIcon());
                box.append(this.header());
                signatory.bind('change', function() {
                    $(".signatureHeader",box).replaceWith(view.header());
                });
                box.width(width);
                box.height(height);
            }
            else {
                box.removeClass('signatureBox');
                var img = $("<img alt=''/>");
                box.css("width",width);
                box.css("height",height);
                img.attr('src',placement.field().value());
                img.css("width",width);
                img.attr("width",width);
                img.css("height",height);
                img.attr("height",height);
                box.append(img);
            }
        this.updateSignatoryCSSClass();
            if (this.resizable) {
                if (box.hasClass("ui-resizable")) box.resizable("destroy");
                box.resizable({
                    stop: function(e, ui) {
                        _.each(placement.field().placements(), function(p) {
                            p.fixWHRel(Math.floor(box.width()),Math.floor(box.height()));
                            if(p.typeSetter)
                                p.typeSetter.place();
                        });
                    },
                    resize: function(e, ui) {

                        if (ui.size.width < 44) {
                          ui.size.width = 44;
                          box.width(44);
                        }
                        if (ui.size.height < 22) {
                          ui.size.height = 22;
                          box.height(22);
                        }

                        if (ui.size.height >  ui.size.width) {
                            box.height(ui.size.width);
                        } else if (3 * ui.size.height <  ui.size.width ) {
                            box.height(ui.size.width/3);
                        }

                        if(placement.typeSetter)
                            placement.typeSetter.place();
                    }

                });
                $(".ui-resizable-se",box).css("z-index","0");
            }
            return this;
    }
});

});
