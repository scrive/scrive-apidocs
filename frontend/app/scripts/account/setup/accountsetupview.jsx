var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var Button = require("../../common/button");
var FlashMessages = require("../../../js/flashmessages.js");
var HtmlTextWithSubstitution = require(
  "../../common/htmltextwithsubstitution"
);
var HubSpot = require("../../common/hubspot_service");
var InfoTextInput = require("../../common/infotextinput");
var Submits = require("../../../js/submits.js");
var Track = require("../../common/track");
var Validation = require("../../../js/validation.js");
var ValidationErrorMessageView = require("./validationerrormessageview");

var AccountSetupView = React.createClass({
  propTypes: {
    fstname: React.PropTypes.string,
    sndname: React.PropTypes.string,
    email: React.PropTypes.string,
    company: React.PropTypes.string,
    userid: React.PropTypes.string.isRequired,
    signupmethod: React.PropTypes.string.isRequired,
    position: React.PropTypes.string,
    phone: React.PropTypes.string,
    companyAdmin: React.PropTypes.bool.isRequired,
    invitationId: React.PropTypes.string.isRequired
  },
  getInitialState: function () {
    return {
      fullName: this.fullName(),
      fullNameValid: true,
      company: this.props.company,
      position: this.props.position,
      phone: this.props.phone,
      password: "",
      passwordValid: true,
      passwordError: "",
      repeatPassword: "",
      repeatPasswordValid: true,
      acceptTOS: false,
      acceptTOSValid: true
    };
  },
  componentWillMount: function () {
    this._fullNameValidation = new Validation.NameValidation();
    this._passwordValidation = new Validation.PasswordValidation({
      callback: this.onPasswordValidationFail,
      message: localization.validation.passwordLessThanMinLength,
      message_max: localization.validation.passwordExceedsMaxLength,
      message_digits: localization.validation.passwordNeedsLetterAndDigit
    });
    this._repeatPasswordValidation = new Validation.PasswordEqValidation({
      with: this.repeatPasswordComparator
    });
  },
  componentDidMount: function () {
    Track.track("Visit account setup");

    // this is disabled, because this code in componentDidMount()
    // is run while it's not mounted to DOM, and IE11 doesn't like that
    //this.refs.fullNameInput.focus();

    var link = $(".is-TOS", this.refs.acceptTOSLabel.getDOMNode());
    link.attr("class", "clickable");
    link.attr("target", "_blank");
    link.attr("href", "/terms");
    link.text(" " + link.text());
  },
  fullName: function () {
    var parts = [this.props.fstname];
    if (this.props.sndname) {
      parts.push(this.props.sndname);
    }

    return parts.join(" ");
  },
  repeatPasswordComparator: function () {
    return this.state.password;
  },
  validateAll: function () {
    var result = {
      fullNameValid: this._fullNameValidation.validateData(
        this.state.fullName
      ),
      passwordValid: this._passwordValidation.validateData(
        this.state.password
      ),
      repeatPasswordValid: this._repeatPasswordValidation.validateData(
        this.state.repeatPassword
      )
    };

    this.setState(result);
    return _.all(_.values(result));
  },
  onPasswordValidationFail: function (text, elem, validation) {
    this.setState({passwordError: validation.message()});
  },
  onSubmitSuccess: function (response) {
    var tosDate = new Date();
    if (response.ok === true) {
      new FlashMessages.FlashMessageAfterReload({
        content: localization.accountSetupModal.flashMessageUserActivated,
        type: "success"
      });

      // HS temporarily disabled, because it doesnt work
      // HubSpot.track(HubSpot.FORM_TOS_SUBMIT, {
      //   "signup_method": this.props.signupmethod,
      //   "fullname": this.fullName(),
      //   "firstname": this.state.fstname,
      //   "lastname": this.state.sndname,
      //   "email": this.props.email,
      //   "phone": this.state.phone,
      //   "company": this.state.company,
      //   "jobtitle": this.state.position
      // });

      Track.track_timeout(
        "Sign TOS",
        {},
        function () {
          window.location = response.location;
        },
        1000
      );
    } else if (response.error == "already_active") {
      new FlashMessages.FlashMessage({
        content: localization.accountSetupModal.flashMessageUserAlreadyActivated,
        type: "error"
      });
    } else if (response.error == "reload") {
      this.forceUpdate();
    }
  },
  onFullNameInputChange: function (value) {
    this.setState({fullName: value});
  },
  onCompanyInputChange: function (value) {
    this.setState({company: value});
  },
  onPositionInputChange: function (value) {
    this.setState({position: value});
  },
  onPhoneInputChange: function (value) {
    this.setState({phone: value});
  },
  onPasswordInputChange: function (value) {
    this.setState({password: value});
  },
  onRepeatPasswordInputChange: function (value) {
    this.setState({repeatPassword: value});
  },
  onTOSCheckboxClick: function (event) {
    this.setState({acceptTOS: !this.state.acceptTOS});
  },
  onCreateAccountButtonClick: function () {
    var acceptTOSValid = (this.state.acceptTOS === true);
    this.setState({
      fullNameValid: true,
      passwordValid: true,
      passwordError: "",
      repeatPasswordValid: true,
      acceptTOSValid: acceptTOSValid
    });

    if (acceptTOSValid) {
      if (this.validateAll()) {
        var urlParts = [
          "", "accountsetup", this.props.userid, this.props.invitationId,
          this.props.signupmethod
        ];

        var words = _.filter(this.state.fullName.split(" "), function (word) {
          return word.trim() != "";
        });

        var fstname = words[0];
        if (fstname === undefined) {
          fstname = "";
        }
        var sndname = _.rest(words).join(" ");

        var submit = new Submits.Submit({
          url: urlParts.join("/"),
          method: "POST",
          ajax: true,
          tos: "on",
          fstname: fstname,
          sndname: sndname,
          password: this.state.password,
          password2: this.state.password,
          phone: this.state.phone,
          company: this.state.company,
          position: this.state.position,
          ajaxsuccess: this.onSubmitSuccess
        });
        submit.send();
      }
    }
  },
  render: function () {
    var checkboxClassName = classNames("checkbox", {
      checked: this.state.acceptTOS
    });

    return (
      <div className="short-input-container">
        <div className="short-input-container-body-wrapper">
          <div className="short-input-container-body">
            <div className="position first">
              <InfoTextInput
                ref="fullNameInput"
                className="big-input"
                infotext={localization.account.accountDetails.fullname}
                inputtype="text"
                value={this.state.fullName}
                onChange={this.onFullNameInputChange}
              />
              {!this.state.fullNameValid &&
                <ValidationErrorMessageView ref="fullNameValidationError">
                  {localization.accountSetupModal.modalAccountSetupMissingName}
                </ValidationErrorMessageView>
              }
            </div>
            <div className="position">
              <InfoTextInput
                ref="companyInput"
                className="med-input med-input-left"
                infotext={localization.accountSetupModal.modalAccountSetupCompany}
                inputtype="text"
                readonly={!this.props.companyAdmin}
                value={this.state.company}
                onChange={this.onCompanyInputChange}
              />
              <InfoTextInput
                ref="positionInput"
                className="med-input"
                infotext={localization.accountSetupModal.modalAccountSetupPosition}
                inputtype="text"
                value={this.state.position}
                onChange={this.onPositionInputChange}
              />
            </div>
            <div className="position">
              <InfoTextInput
                ref="phoneInput"
                className="big-input"
                infotext={localization.accountSetupModal.modalAccountSetupPhone}
                inputtype="text"
                value={this.state.phone}
                onChange={this.onPhoneInputChange}
              />
            </div>
            <div className="position">
              <span className="label password-description">
                {localization.accountSetupModal.modalAccountPasswordRequirements}
              </span>
              <InfoTextInput
                ref="passwordInput"
                className="med-input med-input-left"
                infotext={localization.accountSetupModal.modalAccountSetupChoosePassword}
                inputtype="password"
                value={this.state.password}
                onChange={this.onPasswordInputChange}
              />
              <InfoTextInput
                ref="repeatPasswordInput"
                className="med-input"
                infotext={localization.accountSetupModal.modalAccountSetupRepeatPassword}
                inputtype="password"
                value={this.state.repeatPassword}
                onChange={this.onRepeatPasswordInputChange}
              />
              {!this.state.passwordValid &&
                <ValidationErrorMessageView ref="passwordValidationError">
                  {this.state.passwordError}
                </ValidationErrorMessageView>
              }
              {!this.state.repeatPasswordValid &&
                <ValidationErrorMessageView ref="repeatPasswordValidationError">
                  {localization.validation.passwordsDontMatch}
                </ValidationErrorMessageView>
              }
            </div>
            <div className="checkbox-box">
              <div
                ref="acceptTOSCheckbox"
                className={checkboxClassName}
                onClick={this.onTOSCheckboxClick}
              >
                <div className="checkmark" />
              </div>
              <span className="label-with-link">
                <HtmlTextWithSubstitution
                  ref="acceptTOSLabel"
                  secureText={localization.accountSetupModal.modalAccountSetupTOS}
                  onClicks={{
                    "label": this.onTOSCheckboxClick
                  }}
                />
              </span>
              {!this.state.acceptTOSValid &&
                <ValidationErrorMessageView ref="acceptTOSValidationError">
                  {localization.validation.mustAcceptTOS}
                </ValidationErrorMessageView>
              }
            </div>
            <div className="position">
              <Button
                ref="createAccountButton"
                size="small"
                text={localization.signupModal.modalAccountSetupFooter}
                type="main"
                onClick={this.onCreateAccountButtonClick}
              />
            </div>
          </div>
        </div>
      </div>
    );
  }
});

module.exports = AccountSetupView;
