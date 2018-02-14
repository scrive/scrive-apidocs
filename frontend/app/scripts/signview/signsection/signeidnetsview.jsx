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

  module.exports = React.createClass({
    mixins: [TaskMixin],

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

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      name: React.PropTypes.string.isRequired,
      canSign: React.PropTypes.bool.isRequired,
      onReject: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      return {thisDevice: (this.props.thisDevice === undefined) ? true : this.props.thisDevice};
    },

    render: function () {
      var model = this.props.model;
      var canHaveRejectButton = model.hasRejectOption();
      var self = this;
      var ssn = self.props.ssn;
      var name = self.props.name;

      var buttonClass = classNames({
        "button-block": true,
        "inactive": !this.props.canSign
      });

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      var randomFragment = Math.floor((Math.random() * 1000000) + 1);


      return (
        <div className={divClass}>
          <h1>
            <div className="bankid-logo-wrapper" >
              <span className="bankid-logo mobile-no-bankid-logo" />
            </div>
            {localization.docsignview.eleg.bankid.signNOConfirmationTitle}
          </h1>
          {/* if */ name !== "" &&
            <HtmlTextWithSubstitution
              secureText={"<p>" + localization.docsignview.eleg.bankid.signNOConfirmationText + "</p>"}
              subs={{".put-signatory-name-here": name}}
            />
          }
          {/* else */ name === "" &&
            <p>{localization.docsignview.eleg.bankid.signNOConfirmationTextNoName}</p>
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
