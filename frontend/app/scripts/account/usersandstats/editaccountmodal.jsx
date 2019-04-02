var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Select = require("../../common/select");
var Button = require("../../common/button");
var Track = require("../../common/track");
var List = require("../../lists/list");
var jQuery = require("jquery");
var NotEmptyValidation = require("../../../js/validation.js").NotEmptyValidation;
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var Submit = require("../../../js/submits.js").Submit;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Language = require("../../../js/utils/language.js").Language;
var $ = require("jquery");
var Modal = require("../../common/modal");
var InfoTextInput = require("../../common/infotextinput");
var V = require("../../../js/validation.js");

var CreateAccountModal = require("./createaccountmodal");

module.exports = React.createClass({
    propTypes : {
        active: React.PropTypes.bool.isRequired,
        account: React.PropTypes.object,
        account: React.PropTypes.object,
        onClose: React.PropTypes.func.isRequired,
        onAccept: React.PropTypes.func.isRequired
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var account = props.account;
      if (account != this.props.account) {
        return {
          fstname: account.field("fstname"),
          sndname: account.field("sndname"),
          email: account.field("email"),
          phone: account.field("phone"),
          companyposition: account.field("companyposition"),
          personnumber:  account.field("personalnumber"),
          lang:  account.field("lang"),
          changeEmailView: false
        };
      } else if (props.active && !this.props.active) {
        return {
          changeEmailView: false
        };
      } else {
        return {};
      }
    },
    goToChangeEmailView: function() {
      this.setState({changeEmailView : true});
    },
    handleClose: function() {
      this.props.onClose();
    },
    handleAccept: function() {
      if (this.state.changeEmailView) {
        this.handleAcceptEmailChange();
      } else {
        this.handleAcceptDetailsChange();
      }
    },
    handleAcceptEmailChange: function() {
      var self = this;
      if (self.emailValid()) {
         new Submit({
          method: "POST",
          url: "/api/frontend/changeusersemail/" + self.props.account.field("id"),
          ajax: true,
          ajaxsuccess: function(rs) {
            if (rs.sent) {
              new FlashMessage({
                content: localization.account.companyAccounts.editModal.requestSent,
                type: "success"
              });
              self.props.onAccept();
            } else {
              new FlashMessage({
                content: localization.account.companyAccounts.editModal.emailInUse,
                type: "error"
            })
            }
          },
          ajaxerror: function() {
            new FlashMessage({
              content: localization.account.accountDetails.generalError,
              type: "error"
            });
          },
          newemail: self.state.email
        }).sendAjax();
      }
    },
    handleAcceptDetailsChange: function() {
      var self  = this;
      if (!self.validDetails()) {
        if(!this.personNumberValid()) {
          var validation = new V.PersonalNumberValidation();
          validation.validateData(this.state.personnumber);
          new FlashMessage({content: validation.message(), type: "error"});
        } else if(!this.phoneValid()) {
          var validation = new V.PhoneValidation();
          validation.validateData(this.state.phone);
          new FlashMessage({content: validation.message(), type: "error"});
        }
        return;
      } else {
        new Submit({
          method: "POST",
          url: "/api/frontend/updateusersprofile/" + self.props.account.field("id"),
          ajax: true,
          ajaxsuccess: function() {
            new FlashMessage({
              content: localization.account.companyAccounts.editModal.settingsSaved,
              type: "success"
            });
            self.props.onAccept();
          },
          ajaxerror: function() {
            new FlashMessage({
              content: localization.account.accountDetails.generalError,
              type: "error"
            })
          },
          fstname: self.state.fstname,
          sndname: self.state.sndname,
          personalnumber: self.state.personnumber,
          phone: self.state.phone,
          companyposition: self.state.companyposition,
          lang: self.state.lang
        }).sendAjax();
      }
    },
    fstnameValid: function (v) {
      return new V.NameValidation().or(new V.EmptyValidation()).validateData(this.state.fstname);
    },
    onFstnameChange: function(v) {
      this.setState({fstname:v});
    },
    sndnameValid: function (v) {
      return new V.NameValidation().or(new V.EmptyValidation()).validateData(this.state.sndname);
    },
    onSndnameChange: function(v) {
      this.setState({sndname:v});
    },
    emailValid: function (v) {
      return new V.EmailValidation().or(new V.EmptyValidation()).validateData(this.state.email);
    },
    onEmailChange: function(v) {
      this.setState({email:v});
    },
    phoneValid: function (v) {
      return new V.PhoneValidation().or(new V.EmptyValidation()).validateData(this.state.phone);
    },
    onPhoneChange: function(v) {
      this.setState({phone:v});
    },
    personNumberValid: function (v) {
      return new V.PersonalNumberValidation().or(new V.EmptyValidation()).validateData(this.state.personnumber);
    },
    onPersonNumberChange: function(v) {
      this.setState({personnumber:v});
    },
    companyPositionValid: function (v) {
      return new V.PositionValidation().or(new V.EmptyValidation()).validateData(this.state.companyposition);
    },
    onCompanyPositionChange: function(v) {
      this.setState({companyposition:v});
    },
    onLangChange: function(v) {
      this.setState({lang:v});
    },
    validDetails: function() {
      return this.fstnameValid() && this.sndnameValid() && this.phoneValid() && this.personNumberValid() && this.companyPositionValid();
    },
    languages: function () {
      if (!this._languages) {
        var knownLanguages = Language.allLanguagesOptions();
        this._languages = _.sortBy(knownLanguages, function (l) {
          return l.name.toLowerCase();
        });
      }

      return this._languages;
    },
    render: function() {
      var self = this;
      return (
        <div>
          <Modal.Container active={(self.props.active)}>
            <Modal.Header
              title={this.state.changeEmailView ? localization.account.companyAccounts.editModal.changeEmail : localization.account.companyAccounts.editModal.editDetails }
              showClose={true}
              onClose={self.handleClose}
            />
            <Modal.Content>
              { /* if */ (self.props.account && !self.state.changeEmailView) &&
                <div className="editcompanyusermodal">
                  <table>
                    <tbody>
                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.fstname}</label>
                        </td>
                        <td>
                          <InfoTextInput
                            name="fstname"
                            className={!self.fstnameValid() ? "redborder" : ""}
                            value={self.state.fstname}
                            onChange={self.onFstnameChange}
                          />
                        </td>
                      </tr>
                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.sndname}</label>
                        </td>
                        <td>
                          <InfoTextInput
                            name="sndname"
                            className={!self.sndnameValid() ? "redborder" : ""}
                            value={self.state.sndname}
                            onChange={self.onSndnameChange}
                          />
                        </td>
                      </tr>

                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.email}</label>
                        </td>
                        <td>
                          <input
                            type="text"
                            disabled={true}
                            className="emailinput"
                            value={self.state.email}
                          />
                          <Button
                            text={localization.account.companyAccounts.editModal.change}
                            onClick={self.goToChangeEmailView}
                          />
                        </td>
                      </tr>
                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.personnumber}</label>
                        </td>
                        <td>
                          <InfoTextInput
                            name="personalnumber"
                            className={!self.personNumberValid() ? "redborder" : ""}
                            value={self.state.personnumber}
                            onChange={self.onPersonNumberChange}
                          />
                        </td>
                      </tr>

                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.phone}</label>
                        </td>
                        <td>
                          <InfoTextInput
                            name="phone"
                            className={!self.phoneValid() ? "redborder" : ""}
                            value={self.state.phone}
                            onChange={self.onPhoneChange}
                          />
                        </td>
                      </tr>
                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.companyposition}</label>
                        </td>
                        <td>
                          <InfoTextInput
                            name="companyposition"
                            className={!self.companyPositionValid() ? "redborder" : ""}
                            value={self.state.companyposition}
                            onChange={self.onCompanyPositionChange}
                          />
                        </td>
                      </tr>
                      <tr>
                        <td>
                          <label>{localization.account.accountSecurity.lang}</label>
                        </td>
                        <td>
                          <div className='langSwitcher'>
                            <Select
                              width={300}
                              options={this.languages()}
                              isOptionSelected={function(l) {
                                return l.value  ==  self.state.lang;
                              }}
                              onSelect={function(v) {self.onLangChange(v);}}
                            />
                          </div>
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              }
              { /* else if */ (self.props.account && self.state.changeEmailView) &&
                 <div className="editcompanyusermodal">
                  <table>
                    <tbody>
                      <tr>
                        <td>
                          <label>{localization.account.accountDetails.email}</label>
                        </td>
                        <td>
                          <InfoTextInput
                            name="newemail"
                            className={!self.emailValid() ? "redborder" : ""}
                            value={self.state.email}
                            onChange={self.onEmailChange}
                          />
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              }


            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.handleClose} />
              <Modal.AcceptButton
                text={self.state.changeEmailView ?
                  localization.account.companyAccounts.editModal.changeEmail :
                  localization.account.companyAccounts.editModal.save
                }
                onClick={self.handleAccept}
              />
            </Modal.Footer>
          </Modal.Container>


        </div>
      );
    }
});
