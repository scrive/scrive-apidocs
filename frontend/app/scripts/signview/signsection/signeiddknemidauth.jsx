var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var Checkbox = require("../../common/checkbox");
var InfoTextInput = require("../../common/infotextinput");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Track = require("../../common/track");
var TaskMixin = require("../navigation/task_mixin");
var ViewSize = require("../viewsize");
var Task = require("../navigation/task");
var $ = require("jquery");
var Field = require("../../../js/fields.js").Field;
var classNames = require("classnames");
var SignLegalAgreement = require("./signlegalagreement");

  module.exports = React.createClass({
    mixins: [TaskMixin],

    createTasks: function () {
      var tasks = [this.createSignTask()];

      if (this.props.askForSSN) {
        tasks.push(this.createSSNTask());
      }

      return tasks;
    },

    createSignTask: function () {
      var self = this;
      var model = self.props.model;

      return new Task({
        type: "sign",
        onArrowClick: function () {
          self.props.onSign();
        },
        isComplete: function () {
          return !model.document().currentSignatoryCanSign();
        },
        el: $(self.refs.signButton.getDOMNode()),
        onActivate: function () {
        },
        onDeactivate: function () {
        }
      });
    },

    createSSNTask: function () {
      var self = this;
      var ref = this.refs.ssnInput;
      var model = this.props.model;
      return new Task({
        type: "extra-details",
        onArrowClick: function () {
          ref.focus();
        },
        isComplete: function () {
          return !model.askForSSN();
        },
        el: $(ref.getDOMNode())
      });
    },

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      fieldSSN: React.PropTypes.instanceOf(Field),
      name: React.PropTypes.string.isRequired,
      askForSSN: React.PropTypes.bool.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      ssn: React.PropTypes.string.isRequired,
      onReject: React.PropTypes.func.isRequired,
      onForward: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired,
      showLegalText: React.PropTypes.bool.isRequired
    },

    getInitialState: function () {
      return {thisDevice: (this.props.thisDevice === undefined) ? true : this.props.thisDevice};
    },

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var canHaveForwardButton = model.hasForwardOption();
      var self = this;
      var ssn = self.props.ssn;
      var name = self.props.name;
      var fieldSSN = self.props.fieldSSN;
      var askForSSN = model.askForSSN();

      var inputSSNClass = classNames({
        "obligatory-input": true,
        "valid": !askForSSN
      });

      var buttonClass = classNames({
        "button-block": true,
        "inactive": !this.props.canSign
      });

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      var doc = model.document();
      var sig = doc.currentSignatory();
      var confirmationText = localization.docsignview.eleg.bankid.signDKConfirmationText;
      var confirmationTextNoName = localization.docsignview.eleg.bankid.signDKConfirmationTextNoName;
      var logoClass = classNames({"bankid-logo-nets": true, "dk-nemid-logo": true});
      var confirmationTitle = localization.docsignview.eleg.bankid.signDKConfirmationTitle;

      var randomFragment = Math.floor((Math.random() * 1000000) + 1);

      return (
        <div className={divClass}>
          <h1>
            <span className={logoClass}/>
            {confirmationTitle}
          </h1>
          {/* if */ this.props.showLegalText && <SignLegalAgreement /> }
          {/* if */ name !== "" &&
            <HtmlTextWithSubstitution
              secureText={"<p>" + confirmationText + "</p>"}
              subs={{".put-signatory-name-here": name}}
            />
          }
          {/* else */ name === "" &&
            <p>{confirmationTextNoName}</p>
          }
          { /* if */ (sig.dkNemIDCPRAuthenticationToSign() || sig.dkNemIDCVRAuthenticationToSign())
                && this.props.askForSSN &&
            <dl className="ssn-list">
              <dt><label htmlFor="ssn">{localization.eID.idName.cpr}</label></dt>
              <dd>
                <InfoTextInput
                  id="ssn"
                  ref="ssnInput"
                  infotext={localization.eID.infoText.cpr}
                  className={inputSSNClass}
                  value={fieldSSN.value()}
                  onChange={function (value) { fieldSSN.setValue(value); }}
                />
              </dd>
            </dl>
          }

          {/* else */ sig.dkNemIDCPRAuthenticationToSign() && !this.props.askForSSN &&
            <p className="ssn-text">{localization.eID.idName.cpr} <b>{ssn}</b></p>
          }

          {/* else */ sig.dkNemIDCVRAuthenticationToSign() && !this.props.askForSSN &&
            <p className="ssn-text">{localization.eID.idName.cvr} <b>{ssn}</b></p>
          }

          <Button
            ref="signButton"
            type="action"
            id={randomFragment.toString()}
            href={("#" + randomFragment)}
            className={buttonClass}
            onClick={function () { self.props.onSign(); }}
            text={localization.process.signbuttontext}
          />
          {/* if */ canHaveForwardButton &&
            <Button
              ref="forwardButton"
              className="button-block small-button-block"
              text={localization.process.forwardtext}
              onClick={this.props.onForward}
            />
          }
          {/* if */ canHaveRejectButton &&
            <Button
              ref="rejectButton"
              className="button-block small-button-block"
              text={localization.process.rejectbuttontext}
              onClick={this.props.onReject}
            />
          }
        </div>
      );
    }
  });
