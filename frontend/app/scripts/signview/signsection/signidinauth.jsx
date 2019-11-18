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
      canSign: React.PropTypes.bool.isRequired,
      field: React.PropTypes.instanceOf(Field).isRequired,
      onReject: React.PropTypes.func.isRequired,
      onForward: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    createTasks: function () {
      var tasks = [this.createSignTask()];

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

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var canHaveForwardButton = model.hasForwardOption();

      var field = this.props.field;
      var canSign = this.props.canSign;
      var onSign = this.props.onSign;

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
          <h1>{ localization.docsignview.eleg.bankid.signNLConfirmationTitle }</h1>
          <Button
            ref="signButton"
            type="action"
            className={buttonClass}
            text={localization.process.signbuttontext}
            onClick={function (e) { if (canSign) { onSign(e); } }}
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
