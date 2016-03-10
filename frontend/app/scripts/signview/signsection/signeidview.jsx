var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var Checkbox = require("../../common/checkbox");
var InfoTextInput = require("../../common/infotextinput");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var TaskMixin = require("../tasks/task_mixin");
var ViewSize = require("../viewsize");
var PageTask = require("../../../js/tasks.js").PageTask;
var $ = require("jquery");
var Field = require("../../../js/fields.js").Field;

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

      return new PageTask({
        type: "sign",
        onArrowClick: function () {
          self.props.onSign();
        },
        isComplete: function () {
          return !model.document().currentSignatoryCanSign();
        },
        el:  $(self.refs.signButton.getDOMNode()),
        onActivate: function () {
          mixpanel.track("Begin signature task");
        },
        onDeactivate: function () {
          mixpanel.track("Finish signature task");
        }
      });
    },

    createSSNTask: function () {
      var self = this;
      var ref = this.refs.ssnInput;
      var model = this.props.model;

      return new PageTask({
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
      field: React.PropTypes.instanceOf(Field).isRequired,
      name: React.PropTypes.string.isRequired,
      askForSSN: React.PropTypes.bool.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      ssn: React.PropTypes.string.isRequired,
      onReject: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      return {thisDevice: true};
    },

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var self = this;
      var ssn = self.props.ssn;
      var name = self.props.name;
      var field = self.props.field;

      var inputClass = React.addons.classSet({
        "obligatory-input": true,
        "valid": !self.props.model.askForSSN()
      });

      var buttonClass = React.addons.classSet({
        "button-block": true,
        "inactive": !this.props.canSign
      });

      var divClass = React.addons.classSet({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>
            <span className="bankid-logo" />
            {localization.docsignview.eleg.bankid.signConfirmationTitle}
          </h1>
          {/* if */ name !== "" &&
            <HtmlTextWithSubstitution
              secureText={localization.docsignview.eleg.bankid.signConfirmationText}
              subs={{".put-signatory-name-here": name}}
            />
          }
          {/* else */ name === "" &&
            <p>{localization.docsignview.eleg.bankid.signConfirmationTextNoName}</p>
          }
          {/* if */ this.props.askForSSN &&
            <dl className="ssn-list">
              <dt><label htmlFor="ssn">{localization.personalNumber}</label></dt>
              <dd>
                <InfoTextInput
                  id="ssn"
                  ref="ssnInput"
                  inputtype="number"
                  infotext={localization.ssnInfoText}
                  className={inputClass}
                  value={field.value()}
                  onChange={function (value) { field.setValue(value); }}
                />
              </dd>
            </dl>
          }
          {/* else */ !this.props.askForSSN &&
            <p className="ssn-text">{localization.personalNumber} <b>{ssn}</b></p>
          }
          <Checkbox
            className="large-checkbox"
            label={localization.openBankId}
            checked={this.state.thisDevice}
            onChange={function () { self.setState({thisDevice: !self.state.thisDevice}); }}
          />
          <Button
            ref="signButton"
            type="action"
            className={buttonClass}
            onClick={function () { self.props.onSign(self.state.thisDevice); }}
            text={localization.process.signbuttontext}
          />
          {/* if */ canHaveRejectButton &&
            <Button
              ref="rejectButton"
              className="transparent-button button-block"
              text={localization.process.rejectbuttontext}
              onClick={this.props.onReject}
            />
          }
        </div>
      );
    }
  });
