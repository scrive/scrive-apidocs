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

            div.append(view.arrow());
            div.append(view.instructions());
            div.append(view.checkbox());
            div.append(view.signature());
            div.append(view.text());

            view.$el.html(div.children());

            return view;
        },
        arrow: function() {
            var div = $('<div />');
            div.addClass('design-view-action-document-draggables-arrow');
            div.append($('<img />')
                       .addClass('design-view-action-document-draggables-arrow-img')
                       .attr('src', '/img/dragarrow.png'));
            return div;
        },
        instructions: function() {
            var div = $('<div />');
            div.addClass('design-view-action-document-draggables-instructions');
            div.append($('<div />')
                       .addClass('design-view-action-document-draggables-instructions-text')
                       .text(localization.designview.dAndDactions));
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
            img.attr('src', '/img/checkbox.png');

            // a function because author is not yet defined
            var getcheckbox = function() {
                var field = new Field({fresh: false, 
                                       type: 'checkbox', 
                                       signatory: viewmodel.document().author(),
                                       name: viewmodel.document().newCheckboxName()});
                //viewmodel.document().author().addField(field);
                return field;
            }

            draggebleField(div, getcheckbox);

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
            img.addClass('design-view-action-document-draggables-checkbox-icon');
            img.attr('src', '/img/signature.png');

            var getsignature = function() {
                var signature = new Field({fresh:false,
                                           type:'signature',
                                           signatory: viewmodel.document().author(),
                                           name: viewmodel.document().newSignatureName()});
                return signature;
            };
            draggebleField(div, getsignature);

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
            img.attr('src', '/img/textbox.png');

            var gettext = function() {
                return viewmodel.document().author().field('email', 'standard');
            };

            draggebleField(div, gettext);

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
