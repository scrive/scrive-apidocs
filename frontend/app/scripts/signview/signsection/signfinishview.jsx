var classNames = require("classnames");
var Backbone = require("backbone");
var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require(
  "../../common/htmltextwithsubstitution"
);
var Task = require("../navigation/task");
var TaskMixin = require("../navigation/task_mixin");
var Track = require("../../common/track");
var ViewSize = require("../viewsize");

var SignFinishView = React.createClass({
  mixins: [TaskMixin],
  propTypes: {
    model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
    title: React.PropTypes.string.isRequired,
    name: React.PropTypes.string.isRequired,
    canSign: React.PropTypes.bool.isRequired,
    onSign: React.PropTypes.func.isRequired,
    onReject: React.PropTypes.func.isRequired
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
        self.handleSign();
      },
      isComplete: function () {
        return !model.document().currentSignatoryCanSign();
      },
      el: $(self.refs.signButton.getDOMNode()),
      onActivate: function () {
      },
      onDeactivate: function () {
      }
    })];
  },
  handleSign: function () {
    if (this.props.canSign) {
      this.props.onSign();
    } else {
      this.context.blinkArrow();
    }
  },
  render: function () {
    var self = this;
    var model = this.props.model;
    var doc = model.document();

    var canHaveRejectButton = model.hasRejectOption();

    var buttonClass = classNames({
      "button-block": true,
      "inactive": !this.props.canSign
    });

    var divClass = classNames({
      "col-xs-6": !ViewSize.isSmall(),
      "col-xs-12": ViewSize.isSmall(),
      "center-block": true
    });

    var textSibstitutions = {
      ".put-document-title-here": this.props.title,
      ".put-signatory-name-here": this.props.name
    };

    return (
      <div className={divClass}>
        <h1>{localization.process.signModalTitle}</h1>
        <HtmlTextWithSubstitution
          secureText={localization.signviewConfirmation}
          subs={textSibstitutions}
        />
        <Button
          ref="signButton"
          type="action"
          className={buttonClass}
          text={localization.process.signbuttontext}
          onClick={this.handleSign}
        />
        {/* if */ canHaveRejectButton &&
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
        }
      </div>
    );
  }
});

module.exports = SignFinishView;
