var React = require("react");
var classNames = require("classnames");

var EmailValidation = require("../../../js/validation.js").EmailValidation;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var HubSpot = require("../../common/hubspot_service");
var Language = require("../../../js/utils/language.js").Language;
var Modal = require("../../common/modal");
var NotEmptyValidation = require("../../../js/validation.js").NotEmptyValidation;
var Submit = require("../../../js/submits.js").Submit;
var Select = require("../../common/select");

module.exports = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onClose: React.PropTypes.func.isRequired,
    availableUserGroups: React.PropTypes.isRequired
  },
  getInitialState: function () {
    return {
      fstname: "",
      sndname: "",
      email: "",
      userGroupID: null,
      hasEmailProblem: false
    };
  },
  validateEmail: function () {
    var emailValidator = new NotEmptyValidation({
      message: "Email cannot be empty!"
    });

    emailValidator = emailValidator.concat(new EmailValidation({}));

    return this.state.email.validate(emailValidator);
  },
  onFstnameChange: function (e) {
    this.setState({fstname: e.target.value});
  },
  onSndnameChange: function (e) {
    this.setState({sndname: e.target.value});
  },
  onEmailChange: function (e) {
    this.setState({email: e.target.value});
  },
  onClose: function (reload) {
    this.setState({
      fstname: "",
      sndname: "",
      email: "",
      userGroupID: null,
      hasEmailProblem: false
    });

    this.props.onClose(reload);
  },
  userGroupOptions: function () {
    return this.props.availableUserGroups.map(function (ug) {
      return { name: ug.name, value: ug.id };
    })
  },
  onUserGroupChange: function (v) {
    this.setState({userGroupID: v});
  },
  onAccept: function () {
    var self = this;
    var validEmail = this.validateEmail();

    this.setState({hasEmailProblem: !validEmail});
    var userGroupID = this.state.userGroupID;
    if (this.props.availableUserGroups.length > 1 && !userGroupID) {
      userGroupID = this.props.availableUserGroups[0].id;
    }

    if (validEmail) {
      new Submit({
        url: "/account/companyaccounts/add",
        method: "POST",
        fstname: this.state.fstname,
        sndname: this.state.sndname,
        email: this.state.email,
        user_group_id: userGroupID,
        ajax: true,
        ajaxsuccess: function(resp) {
          if (resp.added) {
            new FlashMessage({
              type: "success",
              content: localization.account.companyAccounts.companyInviteSent
            });
          } else {
            if (resp.samecompany) {
              new FlashMessage({
                type: "success",
                content: localization.account.companyAccounts.companyInviteNotSentSameCompany
              });
            } else {
              new FlashMessage({
                type: "error",
                content: localization.account.companyAccounts.companyInviteNotSent
              });
            }
          }

          // HS temporarily disabled, because it doesnt work
          // HubSpot.track(
          //   HubSpot.FORM_INVITE,
          //   {
          //     "email" : self.state.email,
          //     "language" : Language.current(),
          //     "scrive_domain" : location.hostname,
          //     "signup_method" : "CompanyInvitation",
          //     "firstname" : self.state.fstname,
          //     "lastname" : self.state.sndname
          //   },
          //   true
          // );

          self.onClose(true);
        },
        mixpanel: {
          name: 'Accept',
          props: {
            'Accept' : 'new account'
          }
        }
      }).sendAjax();
    }
  },
  render: function () {
    var emailFieldClassName = classNames({
      "problem": this.state.hasEmailProblem
    });
    var self = this;

    return (
      <Modal.Container active={this.props.active} width={533}>
        <Modal.Header
          title={localization.account.companyAccounts.createNewModalTitle}
          onClose={this.onClose}
          showClose={true}
        />
        <Modal.Content>
          <div>
            <div className="modal-subtitle">{localization.account.companyAccounts.createNewModalBody}</div>
            <div className="standard-input-table">
              <table>
                <tr>
                  <td>{localization.fstname}</td>
                  <td>
                    <input
                      type="text"
                      autoComplete="off"
                      value={this.state.fstname}
                      onChange={this.onFstnameChange}
                    /></td>
                </tr>
                <tr>
                  <td>{localization.sndname}</td>
                  <td>
                    <input
                      type="text"
                      autoComplete="off"
                      value={this.state.sndname}
                      onChange={this.onSndnameChange}
                    /></td>
                </tr>
                <tr>
                  <td>{localization.email}</td>
                  <td>
                    <input
                      className={emailFieldClassName}
                      type="text"
                      autoComplete="off"
                      value={this.state.email}
                      onChange={this.onEmailChange}
                    />
                  </td>
                </tr>
                { this.userGroupOptions().length > 1 &&
                <tr>
                  <td>
                    <label>{localization.company}</label>
                  </td>
                  <td>
                    <div className='langSwitcher'>
                      <Select
                        width={328}
                        options={this.userGroupOptions()}
                        isOptionSelected={function(option) {
                          return option.value  ==  self.state.userGroupID;
                        }}
                        onSelect={function(v) {self.onUserGroupChange(v);}}
                      />
                    </div>
                  </td>
                </tr>
                }
              </table>
            </div>
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.onClose} />
          <Modal.AcceptButton
            text={localization.account.companyAccounts.createNewModalAcceptButton}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    )
  }
});
