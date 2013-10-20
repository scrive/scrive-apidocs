/* 
 * This handles the first time user experience,  
 */

(function(window){

var FirstTimeUserExperienceModel = Backbone.Model.extend({
    progressbar: function() {
        return this.get("progressbar");
    },
    designview: function() {
        return this.get("designview");
    },
    highlight: function() {
        return this.get("highlight");
    }
});

 /*
  * Expected model: FirstTimeUserExperienceModel.
  */
var FirstTimeUserExperienceView = Backbone.View.extend({
    initialize: function (args) {
        this.model = args.model;

        _.bindAll(this, "render", "instructToAddParty", "instructToEnterDetails", "instructToSign", "stopTour");

        this.model.designview().bind('visibility:designviewtab', this.instructToAddParty);
        this.model.designview().bind('visibility:participantopen', this.instructToEnterDetails);
        this.model.designview().bind('visibility:participantclosed', this.instructToSign);
        this.model.designview().bind('visibility:signconfirmation', this.stopTour);

        this.render();
    },
    instructToAddParty: function() {
        var document = this.model.designview().document();
        var that = this;

        if (document.mainfile() != null && document.signatories().length == 1 && this.acceptedSampleDocument) {
            var hasHelpingButtonsExperiment = new Experiment({
              namespace: 'ftue', 
              name: 'extrapartybuttons', 
              domain: ['shown', 'hidden']
            });
            mixpanel.register({'Extra add party buttons shown': hasHelpingButtonsExperiment.value()});
            mixpanel.track("Instructions to add party shown");

            var addParticipant = function(name, email, phone, company) {
                var newSignatory = new Signatory({
                    document: document,
                    signs: true
                });

                // TODO can I set this in a better way?
                newSignatory.emailField().setValue(email);
                if (phone) newSignatory.mobileField().setValue(phone);
                if (company) newSignatory.companyField().setValue(company);
                newSignatory.fstnameField().setValue(name.substring(0, name.indexOf(' '))); 
                newSignatory.sndnameField().setValue(name.substring(name.indexOf(' '))); 
                document.addExistingSignatory(newSignatory);

                // Show participant 
                that.model.designview().setParticipantDetail(newSignatory);
            };

            // Create some buttons that will go in the step explanation.
            var addYourselfButton = new Button({
                color: 'green',
                text: localization.ftue.addYourself,
                onClick: function() {
                    // In normal circumstances, you can't add yourself, and you're not supposed to.
                    // However, emails are not case sensitive, so we can uppercase the first character
                    // of the senders email (which is always lowercase).
                    var newEmail = capitaliseFirstLetter(document.signatories()[0].email()); 
                    var name = document.signatories()[0].name();

                    mixpanel.track('Click add signatory', {'ftuebutton': 'Add yourself'});
                    that.userAddedSelf = true;
                    addParticipant(name, newEmail);
                }
            });

            var addAScriverButton = new Button({
                color: 'green',
                text: localization.ftue.addAScriver,
                onClick: function() {
                    mixpanel.track('Click add signatory', {'ftuebutton': 'Add someone at Scrive'});
                    that.userAddedScriver = true;
                    addParticipant('Viktor Wrede', 'viktor@scrive.com', '+46708884749', 'Scrive AB');
                }
            });

            var buttons = $('<div class="add-participant-buttons"></div>');
            var buttonWrapper = $('<span class="add-participant-button-wrapper"></span>');

            buttons
              .append(buttonWrapper.clone().append(addYourselfButton.el()))
              .append(buttonWrapper.css('margin-left', '20px').append(addAScriverButton.el()));

            var content = $('<div></div>');
            content.append($('<h1 class="arrowed"></h1>').text(localization.ftue.step2));
            content.append($('<h3></h3>').html(localization.ftue.step2description)); 

            var hasHelpingButtons = hasHelpingButtonsExperiment.value() == 'shown';
            if (hasHelpingButtons) { content.append(buttons); }

            this.model.highlight().moveTo({
                el: $('.design-view-action-participant-new-single .button'), 
                margins: {top: 15, left: 15, bottom: 40, right: 55},
                explanation:
                hasHelpingButtons ? {placement: 'left', content: content, height: '180', width: '408'} :
                                    {placement: 'left', content: content, height: '96', width: '290'}
            });
            this.model.progressbar().setStep(2);
        }
    },
    instructToEnterDetails: function() {
        var document = this.model.designview().document();

        if (document.mainfile() != null && document.signatories().length == 2) {
            var that = this;

            var content = $('<div></div>');
            content.append($('<h1 class="arrowed"></h1>').text(localization.ftue.step3));

            var subtitle = localization.ftue.step3description;

            if (this.userAddedSelf) { subtitle = localization.ftue.step3descriptionYourself; }
            if (this.userAddedScriver) { subtitle = localization.ftue.step3descriptionScriver; }

            content.append($('<h3></h3>').html(subtitle)); 

            $('.design-view-action-participant:eq(1) .design-view-action-participant-details-participation').hide(0);

            this.model.highlight().moveTo({
                el: $('.design-view-action-participant:eq(1)'), 
                margins: {top: -60, left: 0, bottom: 80, right: -10},
                explanation: {placement: 'above', content: content, height: '100', width: '795'}
            });
            this.model.progressbar().setStep(3);

            // Force a scroll to have the custom scroll bar inside participants
            // update and hilight to re-align properly. 
            // When the hiding of participants details goes, so does this.
            // 350 is synced well with the expansion of the participant details.
            _.delay(function() {
                $(window).scroll();
            }, 350);

        }
    },
    instructToSign: function() {
        var document = this.model.designview().document();
    
        if (document.mainfile() != null && document.signatories().length == 2 &&
            document.signatories()[1].emailField().isValid()
            ) {

                var content = $('<div class="signing-instruction"></div>');
                content.append($('<h1 class="arrowed"></h1>').text(localization.ftue.step4));
                content.append($('<h3></h3>').html(localization.ftue.step4description));
                this.model.highlight().moveTo({
                    el: $('.sendButton'), 
                    margins: {top: 15, left: 13, bottom: 35, right: 53},
                    explanation: {placement: 'above', content: content, height: '100', width: '230'}
                });
                this.model.progressbar().setStep(4);
        } else if (document.signatories().length == 2 && !document.signatories()[1].emailField().isValid()) {
            // Someone closed the participant, but the email is not correctly filled in.
            // An assumption is being made here that no-one will try to deliver via sms, 
            // but we will track this in mixpanel to see if anyone tries to change delivery method.
            this.model.designview().setParticipantDetail(document.signatories()[1]);
            this.instructToEnterDetails();
        }
    },
    stopTour: function() {
        this.model.progressbar().hide();
        this.model.highlight().hide();
    },
    render: function () {
        var view = this;
        var model = this.model;
        var document = this.model.designview().document();

        this.renderPreviewOfSampleDocument();

        var content = $('<div></div>');
        content.append($('<h1 class="arrowed"></h1>').text(localization.ftue.step1));
        content.append($('<h3></h3>').html(localization.ftue.step1description));
        var highlight = new Highlight({
            el: $('.sample-document-preview'), 
            margins: {top: 0, left: 0, bottom: 30, right: 30},
            explanation: {placement: 'above', content: content, height: '100', width: '330'}
        });

        this.model.set({'highlight': highlight});

        $('body').append(this.model.progressbar().el());
      
        return this;
    },
    renderPreviewOfSampleDocument: function() {
        var that = this;

        if (!this.previewSampleDocument) {
            this.previewSampleDocument = $('<div class="sample-document-preview"></div>');
            this.previewSampleDocument.appendTo('body');
        }

        this.previewSampleDocument.empty();

        var button = new Button({
            color: 'green',
            size: 'big',
            text: localization.ftue.useSampleDocument,
            labelstyle: 'width: 202px',
            onClick: function() {
                mixpanel.track('Sample document accept');
                that.previewSampleDocument.hide();
                that.acceptedSampleDocument = true;
                that.instructToAddParty();
            }
        });

        var sampleDocumentExperiment = new Experiment({namespace: 'welcome', name: 'sampledocument'});

        var img = $('<img class="document-preview" width="250" />');
        img.attr('src', '/img/sample_document_' + sampleDocumentExperiment.value() + '_' + localization.code + '.png'); 

        this.previewSampleDocument.append(button.el()).append(img);
    }
});

window.DesignViewTour = function(args) {
    var model = new FirstTimeUserExperienceModel({
        progressbar: new ProgressBar(),
        designview: args.model
    });

    var view = new FirstTimeUserExperienceView({
        model: model, 
        el: $("<div/>")
    });

    this.el = function() {return $(view.el);};
};

window.FirstTimeUserExperience = {
    isActive: function() {
        return SessionStorage.get('welcome', 'accepted') === 'true';
    },
    reset: function() {
        SessionStorage.set('welcome', 'accepted', false);
    }
};

})(window);
