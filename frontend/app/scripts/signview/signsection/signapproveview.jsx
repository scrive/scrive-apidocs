var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var Track = require("../../common/track");
var TaskMixin = require("../navigation/task_mixin");
var ViewSize = require("../viewsize");
var Task = require("../navigation/task");
var $ = require("jquery");
var classNames = require("classnames");

  module.exports = React.createClass({
    displayName: "SignApproveView",
    mixins: [TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      canApprove: React.PropTypes.bool.isRequired,
      onApprove: React.PropTypes.func.isRequired,
      onReject: React.PropTypes.func.isRequired,
      onForward: React.PropTypes.func.isRequired
    },

    contextTypes: {
      blinkArrow: React.PropTypes.func
    },

    createTasks: function () {
      var self = this;
      var model = self.props.model;

      return [new Task({
        type: "sign",
        onArrowClick: function () {
          self.handleApprove();
        },
        isComplete: function () {
          return !model.document().currentSignatoryCanSignOrApprove();
        },
        el: $(self.refs.approveButton.getDOMNode()),
        onActivate: function () {
        },
        onDeactivate: function () {
        }
      })];
    },

    handleApprove: function () {
      if (this.props.canApprove) {
        this.props.onApprove();
      } else {
        this.context.blinkArrow();
      }
    },

    render: function () {
      var self = this;
      var model = this.props.model;
      var doc = model.document();
      var sig = doc.currentSignatory();
      var canHaveForwardButton = model.hasForwardOption();

      var buttonClass = classNames({
        "button-block": true,
        "inactive": !this.props.canApprove
      });

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <Button
            ref="approveButton"
            type="action"
            className={buttonClass}
            text={localization.process.approvebuttontext}
            onClick={this.handleApprove}
          />
          {/* if */ canHaveForwardButton &&
            <Button
              ref="forwardButton"
              className="button-block small-button-block"
              text={localization.process.forwardtext}
              onClick={this.props.onForward}
            />
          }
          <Button
            ref="rejectButton"
            className="button-block small-button-block"
            text={
              doc.allowrejectreason() ?
                localization.process.rejectbuttontext :
                localization.process.rejectbuttontextwithoutreason
            }
            onClick={this.props.onReject}
          />
        </div>
      );
    }
  });
