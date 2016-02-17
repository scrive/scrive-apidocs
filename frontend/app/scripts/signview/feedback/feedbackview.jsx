var React = require("react");
var Question = require("./questionview");
var HubSpot = require("../../common/hubspot_service");
  var QUESTIONS = {
    Q1: {
      title: localization.feedbackQ1,
      buttons: [{text: localization.feedbackBad, value: "bad"}, {text: localization.feedbackGood, value: "good"}],
      next: {bad: "Q2", good: "Q3"}
    },
    Q2: {
      title: localization.feedbackQ2Title,
      subtitle: localization.feedbackQ2Subtitle,
      buttons: [{text: localization.feedbackSkip, value: "skip"}, {text: localization.feedbackSend, value: "send"}],
      field: "textarea",
      fieldTitle: localization.feedbackFeedback,
      next: {skip: "Q7", send: "Q7"}
    },
    Q3: {
      title: localization.feedbackQ3Title,
      subtitle: localization.feedbackQ3Subtitle,
      buttons: [{text: localization.feedbackNever, value: "never"},
        {text: localization.feedbackSometimes, value: "sometimes"},
        {text: localization.feedbackOften, value: "often"}],
      next: {never: "Q5", sometimes: "Q4", often: "Q4"}
    },
    Q4: {
      title: localization.feedbackQ4Title,
      subtitle: localization.feedbackQ4Subtitle,
      buttons: [{text: localization.feedbackNo, value: "no"}, {text: localization.feedbackYesPlease, value: "yes"}],
      field: "phone",
      fieldTitle: localization.feedbackPhoneNumber,
      next: {no: "Q7", yes: "Q6"}
    },
    Q5: {
      title: localization.feedbackQ5Title,
      subtitle: localization.feedbackQ5Subtitle,
      buttons: [{text: localization.feedbackNo, value: "no"}, {text: localization.feedbackYesLink, value: "yes"}],
      next: {no: "Q7", yes: "Q7"}
    },
    Q6: {
      title: localization.feedbackQ6Title,
      subtitle: localization.feedbackQ6Subtitle,
      buttons: []
    },
    Q7: {
      title: localization.feedbackQ7Title,
      subtitle: localization.feedbackQ7Subtitle,
      buttons: []
    }
  };

  module.exports = React.createClass({
    _phoneNumber: undefined,
    _feedback: undefined,

    getInitialState: function () {
      return {question: "Q1"};
    },

    propTypes: {
      document: React.PropTypes.object.isRequired
    },

    mixpanelProperties: function () {
      var document = this.props.document;
      var properties = {
        "Full Name": document.currentSignatory().name(),
        "$first_name": document.currentSignatory().fstname(),
        "$last_name": document.currentSignatory().sndname(),
        "$email": document.currentSignatory().email(),
        "Language": document.lang().simpleCode(),
        "Company Name": document.currentSignatory().company(),
        "Referring Company": document.author().company(),
        "Signup Method": "BySigning"
      };

      if (this._phoneNumber) {
        properties.Phone = this._phoneNumber;
        properties["Questionnaire Lead"] = true;
      }

      return properties;
    },

    registerInMixpanel: function (extraProperties) {
      var props = _.extend(this.mixpanelProperties(), extraProperties);
      var mail = props.$email;
      mixpanel.alias(mail);
      mixpanel.identify(mail);
      mixpanel.register(props);
      mixpanel.people.set(props);
      mixpanel.track("Questionnaire Done");
    },

    onChangeQuestion: function (event, from, to, text) {
      var endPoints = ["Q7", "Q6"];
      var isDone = endPoints.indexOf(to) > -1;

      if (from === "Q1" && event === "good") {
        mixpanel.track("Questionnaire #1 Accept");
      }

      if (from === "Q3") {
        if (event === "sometimes" || event == "often") {
          mixpanel.track("Questionnaire #2 Accept");
        } else {
          mixpanel.track("Questionnaire #2 Deny");
        }
      }

      if (from === "Q4" && event === "yes") {
        this._phoneNumber = text;
      }

      if (from === "Q2" && event === "send") {
        this._feedback = text;
      }

      if (isDone) {
        var doc = this.props.document;

        var extraMixpanelProperties = {};

        var hubspotData = {
          fullname: doc.currentSignatory().name(),
          firstname: doc.currentSignatory().fstname(),
          lastname: doc.currentSignatory().sndname(),
          email: doc.currentSignatory().email(),
          company: doc.currentSignatory().company(),
          referring_company: doc.author().company(),
          signup_method: "BySigning",
          scrive_domain: location.hostname,
          phone: this._phoneNumber,
          feedback: this.feedback,
          language: doc.lang().simpleCode()
        };

        if (from === "Q4" && event === "yes") {
          HubSpot.track(HubSpot.FORM_YES_SENDS_DOCS, hubspotData);
        }

        if (from === "Q5" && event === "yes") {
          extraMixpanelProperties["Others in your organisation information wanted"] = true;
          HubSpot.track(HubSpot.FORM_NO_SENDS_DOCS, hubspotData);
        }

        if (from !== "Q2") {
          self.registerInMixpanel(extraMixpanelProperties);
        }
      }
    },

    handleClick: function (value, text) {
      var question = this.state.question;
      var data = QUESTIONS[question];
      var next = data.next;

      if (!next[value]) {
        throw new Error(value + " not found in " + question + "'s next");
      }

      this.setState({question: next[value]});
      this.onChangeQuestion(value, question, next[value], text);
    },

    render: function () {
      var question = this.state.question;
      var data = QUESTIONS[question];

      if (!data) {
        throw new Error("question " + question + " not found");
      }

      return (
        <div className="section feedback">
          <Question
            title={data.title}
            subtitle={data.subtitle}
            buttons={data.buttons}
            field={data.field}
            fieldTitle={data.fieldTitle}
            onClick={this.handleClick}
          />
        </div>
      );
    }
  });
