var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var TaskMixin = require("../navigation/task_mixin");
var ViewSize = require("../viewsize");
var Field = require("../../../js/fields.js").Field;
var Task = require("../navigation/task");
var $ = require("jquery");
var classNames = require("classnames");
var SignLegalAgreement = require("./signlegalagreement");

  module.exports = React.createClass({
    mixins: [TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      canSign: React.PropTypes.bool.isRequired,
      field: React.PropTypes.instanceOf(Field).isRequired,
      showLegalText: React.PropTypes.bool.isRequired,
      onReject: React.PropTypes.func.isRequired,
      onForward: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      return {signStatus: "choose_signing_method"};
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

      var signame = model.document().currentSignatory().name();

      var buttonClass = classNames({
        "button-block": true,
        "inactive": !canSign
      });

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });
      var logoClass = classNames({"bankid-logo-nets": true, "no-bankid-logo": true});

      return (
        <div className={divClass}>
          <h1>
            <span className={logoClass}/>
            {localization.docsignview.eleg.bankid.signNOConfirmationTitle}
          </h1>
          {/* if */ this.props.showLegalText && <SignLegalAgreement /> }
          {/* if */ signame !== "" &&
            <HtmlTextWithSubstitution
              secureText={"<p>" + localization.docsignview.eleg.bankid.signNOConfirmationText + "</p>"}
              subs={{".put-signatory-name-here": signame}}
            />
          }
          {/* else */ signame === "" &&
            <p>{localization.docsignview.eleg.bankid.signNOConfirmationTextNoName}</p>
          }
          <Button
            ref="signButton"
            type="action"
            className={buttonClass}
            text={localization.process.signbuttontext}
            onClick={function () { if (canSign) { onSign(); } }}
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
