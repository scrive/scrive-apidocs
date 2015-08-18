define(["legacy_code", "Underscore", "Backbone", "React", "common/button",
  "common/infotextinput", "signview/identify/norwegian/norwegianidentifymodel"],
  function (legacy_code, _, Backbone, React, Button, InfoTextInput, NorwegianIdentifyModel) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(NorwegianIdentifyModel).isRequired
    },

    validMobile: function () {
      var mobile = this.props.model.mobileFormatted();
      return mobile.length === 8 && /^\d+$/.test(mobile);
    },

    handleIdentify: function () {
      if (this.props.model.isMobileMode() && !this.validMobile()) {
        return new FlashMessage({
          type: "error",
          content: localization.identifyPhoneError
        });
      }

      this.props.model.identify();
    },

    render: function () {
      var model = this.props.model;

      var buttonStyle = {
        marginTop: "27px"
      };

      var textinputClass = React.addons.classSet({
        "identify-phone-number": true,
        "valid": this.validMobile(),
        "active": model.canEditMobile()
      });

      var buttonDesktopClass = React.addons.classSet({
        "identify-box-button-select": true,
        "identify-box-button-selected": model.isDesktopMode()
      });

      var buttonMobileClass = React.addons.classSet({
        "identify-box-button-select": true,
        "identify-box-button-selected": model.isMobileMode()
      });

      return (
        <span>
          <div className="identify-box-buttons">
            <div
              ref="identify-box-desktop-button"
              className={buttonDesktopClass}
              onClick={function () { model.setDesktopMode()}}
            >
              {localization.identifyBankIdNo}
            </div>
            <div
              ref="identify-box-mobile-button"
              className={buttonMobileClass}
              text={localization.identifyBankIdNoMobile}
              onClick={function () { model.setMobileMode()}}
            >
              {localization.identifyBankIdNoMobile}
            </div>
          </div>
          <div className="identify-box-content">
            {/* if */ model.isDesktopMode() && <span>
              {localization.idNumber} <b>{model.personalnumber()}</b>
            </span>}
            {/* if */ model.isMobileMode() && model.canEditMobile() &&
              <div>
                {localization.identifyPhoneNumber8}
                <InfoTextInput
                  ref="identify-box-phone-input"
                  value={model.mobile()}
                  onChange={function (v) {model.setMobile(v)}}
                  onFocus={this.toggleEditing}
                  onBlur={this.toggleEditing}
                  className={textinputClass}
                />
                <div style={{marginTop: "26px"}}>
                  {localization.identifyDateOfBirth} <b>{model.dateOfBirth()}</b>
                </div>
              </div>
            }
            {/* if */ model.isMobileMode() && !model.canEditMobile() &&
              <div>
                <div>{localization.identifyPhoneNumber} <b>{model.mobile()}</b></div>
                <div>{localization.identifyDateOfBirth} <b>{model.dateOfBirth()}</b></div>
              </div>
            }
            <div className="identify-box-button">
              <Button
                ref="identify-box-identify-button"
                style={buttonStyle}
                size="big"
                type="action"
                text={localization.identifyBankId}
                onClick={this.handleIdentify}
              />
            </div>
          </div>
        </span>
      );
    }
  });
});
