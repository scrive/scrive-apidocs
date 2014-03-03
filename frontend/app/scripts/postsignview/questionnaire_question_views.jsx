/** @jsx React.DOM */

define(['React'], function(React) {
  var expose = {};

  expose.DelayedSignatures = React.createClass({
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

  expose.DemoCta = React.createClass({
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

  expose.Dislike = React.createClass({
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

  expose.LikeEsigning = React.createClass({
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

  expose.OthersInYourOrg = React.createClass({
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
		<div className="button button-green" onClick={this.yesButton}>
		  <div className="label">{ localization.questionnaire.yes }</div>
		</div>
	      </div>
	    </div>
	  </div>
      );
    }
  });

  expose.SendDocuments = React.createClass({
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
o		  <div className="label">{ localization.questionnaire.no }</div>
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

  expose.Done = React.createClass({
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

  return expose;
});
