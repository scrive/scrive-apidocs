/** @jsx React.DOM */

define(['React', 'StateMachine', 'postsignview/questionnaire_question_views','common/hubspot_service'], function(React, StateMachine, QuestionViews, HubSpot) {
  /**
   *  @description
   *  A questionnaire, asking user to answer a couple of questions and
   *  record the answers and user information to Mixpanel.
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

            var document = self.props.document;
            
            // @note(fredrik) The data is the same for both HubSpot paths, it's just
            // that different forms are used.
            var hubspotData = { "fullname" : document.currentSignatory().name(),
                                "firstname" : document.currentSignatory().fstname(),
                                "lastname" : document.currentSignatory().sndname(),
                                "email" : document.currentSignatory().email(),
                                "company": document.currentSignatory().company(),
                                "signup_method": "BySigning",
                                "scrive_domain" : location.hostname };

            if (to === 'Done' && event === 'yes' && from === 'OthersInYourOrg') {
              extraMixpanelProperties['Others in your organisation information wanted'] = true;
              HubSpot.track(hubspotConf.forms.no_sends_docs, hubspotData);
            }

            if (to === 'Done' && event === 'yes' && from === 'DemoCta') {
              // hs call yes_sends_docs sdf
              HubSpot.track(hubspotConf.forms.yes_sends_docs, hubspotData);
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
        'Full Name': document.currentSignatory().name(),
        '$first_name': document.currentSignatory().fstname(),
        '$last_name': document.currentSignatory().sndname(),
        '$email': document.currentSignatory().email(),
        'Language': document.lang().simpleCode(),
        'Company Name': document.currentSignatory().company(),
        'Referring Company': document.author().company(),
        'Signup Method': 'BySigning'
      };

      if(this.phoneNumber) {
        properties['Phone'] = this.phoneNumber;
        properties['Questionnaire Lead'] = true;
      }

      return properties;
    },

    registerInMixpanel: function(extraProperties) {
      var props = _.extend(this.mixpanelProperties(), extraProperties);
      var mail = props['$email'];
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
                    <span
                      dangerouslySetInnerHTML={{__html: localization.questionnaire.heyImViktorFromScrive}}>
                    </span>
                    <br />
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
