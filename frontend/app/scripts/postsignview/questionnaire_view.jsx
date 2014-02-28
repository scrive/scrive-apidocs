/** @jsx React.DOM */

define(['React', 'Backbone', 'StateMachine'], function(React, Backbone, StateMachine) {
  var Questions = {};

  Questions.DelayedSignatures = React.createClass({
    yesButton: function() {
      mixpanel.track('Questionnaire #2 Accept');
      this.props.yesButton();
    },

    noButton: function() {
      mixpanel.track('Questionnaire #2 Deny');
      this.props.noButton();
    },

    render: function() {
      return (
	<div className="inner">
	  <h1>{ localization.questionnaire.great }</h1>
	  <div className="question">
	    <h6>{ localization.questionnaire.sendDown2 }</h6>

	    <div className="yesnobuttons">
	      <div className="button button-gray" onClick={this.noButton}>
		<div className="label">{ localization.questionnaire.no }</div>
	      </div>
	      <div className="button button-green" onClick={this.yesButton}>
		<div className="label">{ localization.questionnaire.yes }</div>
	      </div>
	    </div>
	  </div>
	</div>
      );
    }
  });

  Questions.DemoCta = React.createClass({
    yesButton: function() {
      mixpanel.track('Questionnaire #4 Show Demo');
      this.props.yesButton();
    },

    handleChange: function(event) {
      this.props.setPhoneNumber(event.target.value);
    },

    render: function() {
      return (
          <div className="inner">
            <h1>{ localization.questionnaire.closeFaster }</h1>
            <div className="question">
              <h6 className="yourcompany">
                { localization.questionnaire.easyToSign }<br/>
                <span>
		  { localization.questionnaire.otherBenefitsP1 }
		  <span className="green-text">{ localization.questionnaire.otherBenefitsP2 }</span>
		  { localization.questionnaire.otherBenefitsP3 }
		</span>
              </h6>
              <h6 className="wanttotry">{ localization.questionnaire.seeHowSimple }</h6>

              <div className="yesnobuttons">
                <input type="text" onChange={this.handleChange} className="phoneinput" placeholder={ localization.questionnaire.phoneNumber } />

		<div className="button button-green" onClick={this.yesButton}>
		  <div className="label">{ localization.questionnaire.showDemo }</div>
		</div>
              </div>
	    </div>
	  </div>
      );
    }
  });

  Questions.Dislike = React.createClass({
    sendFeedback: function() {
      mixpanel.track('Unenjoyable e-signing experience', {feedback: this.feedback });
      this.props.noButton();
    },

    handleChange: function(event) {
      this.feedback = event.target.value;
    },

    render: function() {
      return (
          <div className="inner">
            <h1>{ localization.questionnaire.sorry }</h1>
            <div className="question">
              <h6>{ localization.questionnaire.improvements }</h6>
              <textarea onChange={this.handleChange}></textarea>
              <br />

              <div className="button button-sendfeedback button-green" onClick={this.sendFeedback}>
		<div className="label">{ localization.questionnaire.sendFeedback }</div>
	      </div>
	    </div>
	  </div>

      );
    }
  });

  Questions.LikeEsigning = React.createClass({
    yesButton: function() {
      mixpanel.track('Questionnaire #1 Accept');
      this.props.yesButton();
    },

    noButton: function() {
      mixpanel.track('Questionnaire #1 Deny');
      this.props.noButton();
    },

    render: function() {
      return (
	  <div className="inner">
	    <h1>{ localization.questionnaire.quickQuestion }</h1>
	    <div className="question">
	      <h6>{ localization.questionnaire.enjoySigning }</h6>

	      <div className="yesnobuttons">
		<div className="button button-gray" onClick={this.noButton}>
		  <div className="label">{ localization.questionnaire.no }</div>
		</div>
		<div className="button button-green" onClick={this.yesButton}>
		  <div className="label">{ localization.questionnaire.yes }</div>
		</div>
	      </div>
	    </div>
          </div>

      );
    }
  });

  Questions.OthersInYourOrg = React.createClass({
    yesButton: function() {
      mixpanel.track('Questionnaire Others in your organization Accept');
      this.props.yesButton();
    },

    noButton: function() {
      mixpanel.track('Questionnaire Others in your organization Deny');
      this.props.noButton();
    },

    render: function() {
      return (
          <div className="inner">
            <h1>{ localization.questionnaire.lastQuestion }</h1>
            <div className="question">
              <h6>{ localization.questionnaire.anyoneElse }</h6>

	      <div className="yesnobuttons">
		<div className="button button-gray" onClick={this.noButton}>
		  <div className="label">{ localization.questionnaire.no }</div>
		</div>
		<div className="button button-green" onClick={this.props.yesbutton}>
		  <div className="label">{ localization.questionnaire.yes }</div>
		</div>
	      </div>
	    </div>
	  </div>
      );
    }
  });

  Questions.SendDocuments = React.createClass({
    yesButton: function() {
      mixpanel.track('Questionnaire #2 Accept');
      this.props.yesButton();
    },

    noButton: function() {
      mixpanel.track('Questionnaire #2 Deny');
      this.props.noButton();
    },

    render: function() {
      return (
          <div className="inner">
            <h1>{ localization.questionnaire.great }</h1>
            <div className="question">
              <h6>{ localization.questionnaire.sendDownP1 }</h6>
	      <h6>{ localization.questionnaire.sendDownP2 }</h6>

	      <div className="yesnobuttons">
		<div className="button button-gray" onClick={this.noButton}>
		  <div className="label">{ localization.questionnaire.no }</div>
		</div>
		<div className="button button-green" onClick={this.yesButton}>
		  <div className="label">{ localization.questionnaire.yes }</div>
		</div>
	      </div>
            </div>
          </div>
      );
    }
  });

  Questions.Done = React.createClass({
    render: function() {
      return (
	  <div className="inner">
	    <h1>{ localization.questionnaire.thanksForYourTime }</h1>
	    <div className="question">
	      { localization.questionnaire.haveAGreatDay }
	    </div>
	  </div>
      );
    }
  });


  /**
   *  @description
   *  A questionnaire, asking user to answer a couple of questions and
   *  record the answers to Mixpanel.
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

    yesanswer: function() {
      this.questionSTM.yes();
    },

    noanswer: function() {
      this.questionSTM.no();
    },

    /**
     *  @description
     *  Set phoneNumber on internal object, which is then used
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
      // Which question to display is determined by the StateMachine,
      // by setting this.state.question
      var Question = Questions[this.state.question];

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
                  <b className="notch"></b>
                </div>
              </div>

              <div>
                <div className="questions">
		  <Question yesButton={this.yesanswer} noButton={this.noanswer} setPhoneNumber={this.setPhoneNumber} />
		</div>
	      </div>
	    </div>
	  </div>
      );
    }
  });

});
