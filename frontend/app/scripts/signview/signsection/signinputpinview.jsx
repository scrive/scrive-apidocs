define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/infotextinput",
  "common/htmltextwithsubstitution"],
  function (legacy_code, _, Backbone, React, Button, InfoTextInput, HtmlTextWithSubstitution) {

  return React.createClass({
    propTypes: {
      title: React.PropTypes.string.isRequired,
      name: React.PropTypes.string.isRequired,
      onBack: React.PropTypes.func.isRequired,
      onSign: React.PropTypes.func.isRequired
    },

    render: function () {
      var self = this;

      var divClass = React.addons.classSet({
        "col-xs-6": !BrowserInfo.isSmallScreen(),
        "col-xs-12": BrowserInfo.isSmallScreen(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1>{localization.process.signbuttontext}</h1>
          <p>
            <HtmlTextWithSubstitution
              subs={{".put-document-title-here": this.props.title, ".put-signatory-name-here": this.props.name}}
              secureText={localization.signviewConfirmation}
            />
          </p>
          <dl>
            <dt><label htmlFor="pin">{localization.docsignview.pinSigning.enterSMSPin}</label></dt>
            <dd>
              <InfoTextInput
                infotext={localization.docsignview.pinSigning.checkYourPhone}
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
});
