var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var InfoTextInput = require("../../common/infotextinput");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var ViewSize = require("../viewsize");
var classNames = require("classnames");
var SignHeader = require("./signheader");

  module.exports = React.createClass({
    propTypes: {
      title: React.PropTypes.string.isRequired,
      name: React.PropTypes.string.isRequired,
      onBack: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired,
      showLegalText: React.PropTypes.bool.isRequired
    },

    render: function () {
      var self = this;

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <SignHeader
            title={this.props.title}
            name={this.props.name}
            showLegalText={this.props.showLegalText}
          />
          <dl>
            <dt><label htmlFor="pin">{localization.docsignview.pinSigning.enterSMSPin}</label></dt>
            <dd>
              <InfoTextInput
                infotext={localization.docsignview.pinSigning.checkYourPhone}
                inputtype="number"
                id="pin"
                className="obligatory-input"
                ref="pin"
              />
            </dd>
          </dl>
          <Button
            type="action"
            className="button-block"
            onClick={function () { self.props.onSign(self.refs.pin.value()); }}
            text={localization.process.signbuttontext}
          />
          <Button
            className="transparent-button button-block"
            text={localization.toStart.backFromSigningPage}
            onClick={this.props.onBack}
          />
        </div>
      );
    }
  });
