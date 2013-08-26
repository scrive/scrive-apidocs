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
            var div = $("<div class='design-view-action-document-draggables-help help1'/>");
            div.append($("<div class='wrapper'>")
              .append($("<div class='icon'/>"))
              .append($("<span class='text'/>").text(localization.designview.draggablehelp1)));
            return div;
        },
        help2: function() {
            var div = $("<div class='design-view-action-document-draggables-help help2'/>");
            div.append($("<div class='wrapper'>")
              .append($("<div class='icon' />"))
              .append($("<span class='text'/>").text(localization.designview.draggablehelp2)));


            return div;
        },
	createDraggable: function(fieldOrPlacementFnValues, buttonText, cssClass) {
	    var div = $("<div class='design-view-action-document-draggable design-view-action-document-draggable-" + cssClass + "' />");
            var wra = $("<div class='design-view-action-document-draggable-wrapper'/>");
            var innerWrapper = $("<div class='design-view-action-document-draggable-inner-wrapper'/>");
	    var iconWrapper = $("<div class='design-view-action-document-draggable-icon-wrapper' />");
            var imgdiv = $("<div class='design-view-action-document-draggable-icon' />");
	    var txt = $("<div class='design-view-action-document-draggable-text'/>");
	    
	    var fieldOrPlacementFN = function() {
                return new Field(fieldOrPlacementFnValues);
            };

            draggebleField(div, fieldOrPlacementFN, undefined, undefined, true);

            div.append(wra);
            wra.append(innerWrapper);
	    innerWrapper.append(iconWrapper);
	    innerWrapper.append(txt);
            iconWrapper.append(imgdiv);
            txt.append('<span>' + buttonText + '</span>');

            return div;
	},
        checkbox: function() {
	    return this.createDraggable({fresh: false,
                                       type: 'checkbox',
                                       signatory: this.model.document().author(),
                                       name: this.model.document().newCheckboxName()},
					localization.designview.checkbox, 'checkbox');
        },
        signature: function() {
	    return this.createDraggable({fresh:false,
                                           type:'signature',
                                           signatory: this.model.document().author(),
                                           name: this.model.document().newSignatureName()},
					localization.designview.signatureBox, 'signature');
					
        },
        text: function() {
	    return this.createDraggable({signatory: this.model.document().author(),
					 name: 'fake',
					 type: 'fake',
					 value: localization.designview.freeTextBox},
					localization.designview.freeTextBox, 'textbox');
	}
	});



    window.DesignViewDraggablesView = function(args) {
        return new DesignViewDraggablesView(args);
    };

}(window));
