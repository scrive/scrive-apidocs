/*
  Anything you see in the second tab in design view.

*/

(function(window) {
    // expected model: DesignViewModel
    var DesignViewDraggablesView = Backbone.View.extend({
        className: 'design-view-action-document-draggables',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
            view.model.document().bind('change:signatories', view.render);
        },
        render: function() {
            var view = this;

            var div = $('<div />');

            div.append(view.help1());
            div.append(view.help2());
            div.append(view.checkbox());
            div.append(view.signature());
            div.append(view.text());

            view.$el.html(div.children());

            return view;
        },
        help1: function() {
            var div = $("<div class='design-view-action-document-draggables-help'/>");
            div.append($("<div class='wrapper'>")
              .append($("<span class='number'/>").text("1."))
              .append($("<span class='text'/>").text(localization.designview.draggablehelp1))
              .append($("<img src='/img/place-fields-help1.png'/>")));
            return div;
        },
        help2: function() {
            var div = $("<div class='design-view-action-document-draggables-help' style='margin-right:80px'/>");
            div.append($("<div class='wrapper'>")
              .append($("<span class='number'/>").text("2."))
              .append($("<span class='text'/>").text(localization.designview.draggablehelp2))
              .append($("<img src='/img/place-fields-help2.png'/>")));


            return div;
        },
        checkbox: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $('<div />');
            div.addClass('design-view-action-document-draggables-checkbox');
            var wra = $('<div />');
            wra.addClass('design-view-action-document-draggables-checkbox-wrapper');
            var txt = $('<div />');
            txt.addClass('design-view-action-document-draggables-checkbox-text');

            var img = $('<img />');
            img.addClass('design-view-action-document-draggables-checkbox-icon');
            img.attr('src', '/img/place-fields-checkbox.png');

            // a function because author is not yet defined
            var getcheckbox = function() {
                var field = new Field({fresh: false,
                                       type: 'checkbox',
                                       signatory: viewmodel.document().author(),
                                       name: viewmodel.document().newCheckboxName()});
                //viewmodel.document().author().addField(field);
                return field;
            }

            draggebleField(div, getcheckbox,undefined, undefined,true);

            div.append(wra);
            wra.append(txt);
            txt.append(img);
            txt.append(localization.designview.checkbox);

            return div;
        },
        signature: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();
            var author = doc.author();

            var div = $('<div />');
            div.addClass('design-view-action-document-draggables-signature');
            var wra = $('<div />');
            wra.addClass('design-view-action-document-draggables-signature-wrapper');
            var txt = $('<div />');
            txt.addClass('design-view-action-document-draggables-signature-text');

            var img = $('<img />');
            img.addClass('design-view-action-document-draggables-signature-icon');
            img.attr('src', '/img/place-fields-signaturebox.png');

            var getsignature = function() {
                var signature = new Field({fresh:false,
                                           type:'signature',
                                           signatory: viewmodel.document().author(),
                                           name: viewmodel.document().newSignatureName()});
                return signature;
            };
            draggebleField(div, getsignature,undefined, undefined,true);

            div.append(wra);
            wra.append(txt);
            txt.append(img);
            txt.append(localization.designview.signatureBox);

            return div;
        },
        text: function() {
            var view      = this;
            var viewmodel = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-document-draggables-textbox');
            var wra = $('<div />');
            wra.addClass('design-view-action-document-draggables-textbox-wrapper');
            var txt = $('<div />');
            txt.addClass('design-view-action-document-draggables-textbox-text');

            var img = $('<img />');
            img.addClass('design-view-action-document-draggables-textbox-icon');
            img.attr('src', '/img/place-fields-customfield.png');

            var gettext = function() {
                return new Field({
                    signatory: viewmodel.document().author(),
                    name: 'fake',
                    type: 'fake',
                    value: localization.designview.withoutParticipant
                });
            };

            draggebleField(div, gettext, undefined, undefined, true);

            div.append(wra);
            wra.append(txt);
            txt.append(img);
            txt.append(localization.designview.freeTextBox);

            return div;
        },
        hide: function() {
            var view = this;
            view.$el.toggleClass('hidden', true);
            return view;
        },
        unhide: function() {
            var view = this;
            view.$el.toggleClass('hidden', false);
            return view;
        }
    });



    window.DesignViewDraggablesView = function(args) {
        return new DesignViewDraggablesView(args);
    }

}(window));
