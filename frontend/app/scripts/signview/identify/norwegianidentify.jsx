define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/infotextinput"],
  function (legacy_code, _, Backbone, React, Button, InfoTextInput) {

  return React.createClass({
    propTypes: {
      onIdentify: React.PropTypes.func.isRequired,
      personalNumber: React.PropTypes.string.isRequired,
      phoneNumber: React.PropTypes.string
    },

    getInitialState: function () {
      return {type: "desktop", phone: "", editing: false};
    },

    setDesktop: function () {
      this.setState({type: "desktop"});
    },

    setMobile: function () {
      this.setState({type: "mobile"});
    },

    setPhone: function (value) {
      this.setState({phone: value});
    },

    validPhone: function () {
      var phone = this.state.phone;
      return phone.length === 8 && /^\d+$/.test(phone);
    },

    hasPhone: function () {
      return this.props.phoneNumber ? true : false;
    },

    handleIdentify: function () {
      var isMobile = this.state.type === "mobile";
      var phone = this.state.phone || this.props.phoneNumber;

      if (isMobile && !this.hasPhone() && !this.validPhone()) {
        return new FlashMessage({
          type: "error",
          content: localization.identifyPhoneError
        });
      }

      this.props.onIdentify(this.state.type, phone);
    },

    toggleEditing: function () {
      this.setState({editing: !this.state.editing});
    },

    render: function () {
      var isDesktop = this.state.type === "desktop";
      var isMobile = this.state.type === "mobile";
      var personalNumber = this.props.personalNumber;
      var dateOfBirth = this.props.personalNumber.slice(0, 6);

      var buttonStyle = {
        marginTop: "27px"
      };

      var textinputClass = React.addons.classSet({
        "identify-phone-number": true,
        "valid": this.validPhone(),
        "active": this.state.editing && !this.validPhone()
      });

      return (
        <span>
          <div style={{marginBottom: "30px"}}>
            <Button
              className="identify-box-button-select"
              style={{textDecoration: isDesktop && "underline"}}
              size="big"
              text={localization.identifyBankIdNo}
              onClick={this.setDesktop}
            />
            <Button
              className="identify-box-button-select"
              style={{textDecoration: isMobile && "underline"}}
              size="big"
              text={localization.identifyBankIdNoMobile}
              onClick={this.setMobile}
            />
          </div>
          {isDesktop && <span>
            {localization.idNumber} <b>{personalNumber}</b>
          </span>}
          {isMobile && !this.hasPhone() &&
            <div>
              {localization.identifyPhoneNumber8}
              <InfoTextInput
                value={this.state.phone}
                onChange={this.setPhone}
                onFocus={this.toggleEditing}
                onBlur={this.toggleEditing}
                className={textinputClass}
              />
              <div style={{marginTop: "26px"}}>
                {localization.identifyDateOfBirth} <b>{dateOfBirth}</b>
              </div>
            </div>
          }
          {isMobile && this.hasPhone() &&
            <div>
              <div>{localization.identifyPhoneNumber} <b>{this.props.phoneNumber}</b></div>
              <div>{localization.identifyDateOfBirth} <b>{dateOfBirth}</b></div>
            </div>
          }
          <div className="identify-box-button">
            <Button
              style={buttonStyle}
              size="big"
              type="action"
              text={localization.identifyBankId}
              onClick={this.handleIdentify}
            />
          </div>
        </span>
      );
    }
  });
});
