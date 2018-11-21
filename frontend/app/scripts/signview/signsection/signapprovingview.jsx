var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var ViewSize = require("../viewsize");
var $ = require("jquery");
var classNames = require("classnames");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      title: React.PropTypes.string.isRequired,
      name: React.PropTypes.string.isRequired,
      canApprove: React.PropTypes.bool.isRequired,
      onApprove: React.PropTypes.func.isRequired,
      onBack: React.PropTypes.func.isRequired
    },

    render: function () {
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{localization.process.signModalTitle}</h1>
          <p>
            <HtmlTextWithSubstitution
              secureText={localization.signviewApproveConfirmation}
              subs={{".put-document-title-here": this.props.title, ".put-signatory-name-here": this.props.name}}
            />
          </p>
          <Button
            type="action"
            ref="approveButton"
            className="button-block sign-button"
            onClick={this.props.onApprove}
            text={localization.process.approvebuttontext}
          />
          <Button
            className="transparent-button button-block"
            onClick={this.props.onBack}
            text={localization.toStart.backFromSigningPage}
          />
        </div>
      );
    }
  });
