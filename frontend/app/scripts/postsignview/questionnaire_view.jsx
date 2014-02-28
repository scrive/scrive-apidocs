/** @jsx React.DOM */

define(['React', 'StateMachine', 'postsignview/questionnaire_question_views'], function(React, StateMachine, QuestionViews) {
  /**
   *  @description
   *  A questionnaire, asking user to answer a couple of questions and
   *  record the answers and user informationto Mixpanel.
   */
  return React.createClass({
    componentDidMount: function() {
      this.initStateMachine();
    },

    getInitialState: function() {
      return {question: 'LikeEsigning'};
    },

    /**
     *  @description
     *  StateMachine which determines which question view to display.
     */
    initStateMachine: function() {
      var self = this;
      this.questionSTM = StateMachine.create({
        initial: self.state.question,
        events: [
          { from: 'LikeEsigning', name: 'yes', to: 'SendDocuments' },
          { from: 'LikeEsigning', name: 'no', to: 'Dislike' },
          { from: 'Dislike', name: 'no', to: 'Done' },
          { from: 'SendDocuments', name: 'yes', to: 'DemoCta' },
          { from: 'SendDocuments', name: 'no', to: 'OthersInYourOrg' },
          { from: 'DelayedSignatures', name: 'yes', to: 'DemoCta' },
          { from: 'DelayedSignatures', name: 'no', to: 'OthersInYourOrg' },
          { from: 'OthersInYourOrg', name: 'yes', to: 'Done' },
          { from: 'OthersInYourOrg', name: 'no', to: 'Done' },
          { from: 'DemoCta', name: 'yes', to: 'Done' }
        ],
        callbacks: {
          onchangestate: function(event, from, to) {
	    // change questionview
            self.setState({question: to});
            var extraMixpanelProperties = {};

            if (to === 'Done' && event === 'yes' && from === 'OthersInYourOrg') {
              extraMixpanelProperties['Others in your organisation information wanted'] = true;
            }

            if (to === 'Done') {
              self.registerInMixpanel(extraMixpanelProperties);
            }
          }
        }
      });
    },

    yesAnswer: function() {
      this.questionSTM.yes();
    },

    noAnswer: function() {
      this.questionSTM.no();
    },

    /**
     *  @description
     *  Set phoneNumber on internal object, which is used
     *  when registering current user to Mixpanel
     */
    phoneNumber: null,
    setPhoneNumber: function(phoneNumber) {
      this.phoneNumber = phoneNumber;
    },

    mixpanelProperties: function() {
      var document = this.props.document;
      var properties = {
        saveUrl: document.currentSignatory().saveurl(),
        fullName: document.currentSignatory().name(),
        firstName: document.currentSignatory().fstname(),
        lastName: document.currentSignatory().sndname(),
        email: document.currentSignatory().email(),
        language: document.lang().simpleCode(),
        companyName: document.currentSignatory().company(),
        referringCompany: document.author().company(),
        signupMethod: 'BySigning'
      };

      if(this.phoneNumber) {
	properties['Phone'] = this.phoneNumber;
      }

      return properties;
    },

    registerInMixpanel: function(extraProperties) {
      var props = _.extend(this.mixpanelProperties(), extraProperties);
      var mail = props.email;
      delete props.saveUrl;
      mixpanel.alias(mail);
      mixpanel.identify(mail);
      mixpanel.register(props);
      mixpanel.people.set(props);
      mixpanel.track('Questionnaire Done');
    },

    render: function() {
      // Which Question to display is determined by the StateMachine,
      // by setting this.state.question
      var Question = QuestionViews[this.state.question];

      return (
          <div className="questionnaire-wrapper">
            <div className="questionnaire">
              <div className="hithere">
                <div className="wrapper">
                  <img className="viktor" src="/img/questionnaire/viktor.png" />
                  <div className="heyimviktor">
                    { localization.questionnaire.heyImViktor } <span className="normal-weight">Scrive!</span><br />
                    { localization.questionnaire.ourEsignService }
                  </div>
                  <div className="notch"></div>
                </div>
              </div>

              <div>
                <div className="questions">
		  <Question yesButton={this.yesAnswer} noButton={this.noAnswer} setPhoneNumber={this.setPhoneNumber} />
		</div>
	      </div>
	    </div>
	  </div>
      );
    }
  });

});
