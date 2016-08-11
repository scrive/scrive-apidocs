var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var InfoTextInput = require("../../common/infotextinput");
var Track = require("../../common/track");
var TaskMixin = require("../navigation/task_mixin");
var ViewSize = require("../viewsize");
var Field = require("../../../js/fields.js").Field;
var Task = require("../navigation/task");
var $ = require("jquery");
var classNames = require("classnames");

  module.exports = React.createClass({
    mixins: [TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      askForPhone: React.PropTypes.bool.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      field: React.PropTypes.instanceOf(Field).isRequired,
      onReject: React.PropTypes.func.isRequired,
      onNext: React.PropTypes.func.isRequired
    },

    createTasks: function () {
      var tasks = [this.createSignTask()];

      if (this.props.askForPhone) {
        tasks.push(this.createPhoneTask());
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
          Track.track("Begin signature task");
        },
        onDeactivate: function () {
          Track.track("Finish signature task");
        }
      });
    },

    createPhoneTask: function () {
      var self = this;
      var ref = this.refs.phoneInput;
      var model = this.props.model;

      return new Task({
        type: "extra-details",
        onArrowClick: function () {
          ref.focus();
        },
        isComplete: function () {
          return !model.askForPhone();
        },
        el: $(ref.getDOMNode())
      });
    },

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var field = this.props.field;
      var askForPhone = this.props.askForPhone;
      var canSign = this.props.canSign;
      var onNext = this.props.onNext;

      var inputClass = classNames({
        "obligatory-input": true,
        "valid": !model.askForPhone()
      });

      var buttonClass = classNames({
        "button-block": true,
        "inactive": !canSign
      });

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{localization.docsignview.pinSigning.signWithSMSPin}</h1>
          {/* if */ askForPhone &&
            <dl>
              <dt><label htmlFor="phone">{localization.phonePlaceholder}</label></dt>
              <dd>
                <InfoTextInput
                  id="phone"
                  inputtype="tel"
                  infotext={localization.phoneInfoText}
                  autocomplete={false}
                  ref="phoneInput"
                  className={inputClass}
                  value={field.value()}
                  onChange={function (value) { field.setValue(value); }}
                />
              </dd>
            </dl>
          }
          {/* else */ !askForPhone &&
            <p className="pin-phone">
              {localization.docsignview.pinSigning.pinWillBeSentTo} <b>{field.value()}</b>
            </p>
          }
          <Button
            ref="signButton"
            type="action"
            className={buttonClass}
            text={localization.next}
            onClick={function (e) { if (canSign) { onNext(e); } }}
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
