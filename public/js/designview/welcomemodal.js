/* A simple modal telling the user that their document has been saved
 * and that they can try sending a document if they'd like.
 */

(function(window){

// We need to close the modal from within itself, so we keep track of it.
var popup;

/*
 * The actual welcoming modal. 
 */
var WelcomeInformation = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    trySendingADocument: function() {
        var div = $('<div class="action" />');

        div.append($('<h1/>').text(localization.welcomemodal.whatIsScrive));

        var middleText = $('<div class="scrivecapabilities"/>');
        middleText.append($('<h2 />').html(localization.welcomemodal.possibleWithScrive));
        middleText.append($('<h2/>').html(localization.welcomemodal.trytext));
        div.append(middleText);

        div.append(Button.init({
            text: localization.welcomemodal.trybutton, 
            labelstyle: 'font-size: 22px',
            cssClass: 'trybutton',
            size: 'big',
            color: 'blue',
            onClick: function() {
                mixpanel.track('Welcome modal accept', {}, function() {
                    popup.view.clear();
                });
            }
        }).input());
    
        return div;
    },
    render: function () {
        var view = this;
        this.container = $(this.el);
        this.container.empty();
        this.container.append(view.trySendingADocument());

        mixpanel.track('Welcome modal shown');

        return this;
    }
});

window.WelcomeModal = function(args) { 
    var clickToSee = $('<span />').addClass('modal-subtitle');
    var documentLink = $('<a class="documentlink" href="/d/' + args.documentId + '"></a>'); 
    clickToSee.text(localization.welcomemodal.subtitle + ' ');
    documentLink.text(localization.welcomemodal.subtitlelink);
    clickToSee.append(documentLink);

    mixpanel.track_links('.welcomemodal .documentlink', 'Documend saved document link clicked'); 

    popup = Confirmation.popup({
        title: $('<span/>').text(localization.welcomemodal.title).append($('<br />')).append(clickToSee),
        content: new WelcomeInformation({el: $('<div />')}).el,
        extraClass: 'welcomemodal',
        width: 920,
        footerVisible: false,
        onReject: function() { // go to the archive
            mixpanel.track('Welcome modal closed', {}, function() {
                window.location.href = '/d';
            });
        }
    });
};

})(window);
