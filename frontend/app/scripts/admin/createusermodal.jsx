var React = require("react");

var EmailValidation = require("../../js/validation.js").EmailValidation;
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var NameValidation = require("../../js/validation.js").NameValidation;
var NotEmptyValidation = require("../../js/validation.js").NotEmptyValidation;
var Modal = require("../common/modal");
var Submit = require("../../js/submits.js").Submit;

module.exports = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    companyid: React.PropTypes.string,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      fstname: "",
      fstnameError: false,
      sndname: "",
      sndnameError: false,
      email: "",
      emailError: false,
      lang: "en",
      isadmin: false
    };
  },
  onClose: function (reload) {
    this.setState(this.getInitialState());

    this.props.onClose(reload);
  },
  onAccept: function () {
    var validationResult = true;

    var validationState = {
      fstnameError: false,
      sndnameError: false,
      emailError: false
    };

    if(!new NameValidation().validateData(this.state.fstname) && this.state.fstname !== "") {
      validationState.fstnameError = true;
      validationResult = false;
    }

    if(!new NameValidation().validateData(this.state.sndname) && this.state.sndname !== "") {
      validationState.sndnameError = true;
      validationResult = false;
    }

    if(!new NotEmptyValidation().validateData(this.state.email)
      ||
      !new EmailValidation().validateData(this.state.email)
      ) {
      validationState.emailError = true;
      validationResult = false;
    }

    this.setState(validationState);

    if (validationResult) {
      var self = this;
      var successCallback = function(resp) {
        if (resp.success) {
          self.onClose(true);
        } else {
          new FlashMessage({
            type: 'error', content: resp.error_message
          });
        }
      };

      var submitParams = {
        method: "POST",
        fstname: this.state.fstname,
        sndname:  this.state.sndname,
        email: this.state.email,
        lang: this.state.lang
      };

      if (this.props.companyid) {
        submitParams.url = "/adminonly/companyadmin/users/" + this.props.companyid;
        submitParams.iscompanyadmin = (this.state.isadmin) ? "Yes" : undefined;
      } else {
        submitParams.url = "/adminonly/createuser";
        submitParams.add = "true";
      }

      var submit = new Submit(submitParams).sendAjax(
        successCallback,
        function() {
          window.location.reload();
        }
      );
    }
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
  onLangChange: function (e) {
    this.setState({lang: e.target.value});
  },
  onIsadminChange: function (e) {
    this.setState({isadmin: e.target.checked});
  },
  title: function () {
    if (this.props.companyid) {
      return "Create user in company";
    }

    return "Create user with empty company";
  },
  subtitle: function () {
    if (this.props.companyid) {
      return "Create new user account in company";
    }

    return "Create new user account";
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active}>
        <Modal.Header
          title={this.title()}
          showClose={true}
          onClose={this.onClose}
        />
        <Modal.Content>
          <div className="standard-input-table">
            <p>{this.subtitle()}</p>

            <table>
              <tbody>
                <tr>
                  <td>First name:</td>
                  <td>
                    <input
                      type="text"
                      autoComplete="off"
                      value={this.state.fstname}
                      onChange={this.onFstnameChange}
                    />
                  </td>
                </tr>
                <tr className="validation-tr" style={{display: (this.state.fstnameError) ? '' : 'none'}}>
                  <td></td>
                  <td>
                    <div className="validation-failed-msg">Wrong first name format!</div>
                  </td>
                </tr>              
                <tr>
                  <td>Second name:</td>
                  <td>
                    <input
                      type="text"
                      autoComplete="off"
                      value={this.state.sndname}
                      onChange={this.onSndnameChange}
                    />
                  </td>
                </tr>
                <tr className="validation-tr" style={{display: (this.state.sndnameError) ? '' : 'none'}}>
                  <td></td>
                  <td>
                    <div className="validation-failed-msg">Wrong second name format!</div>
                  </td>
                </tr>
                <tr>
                  <td>Email address:</td>
                  <td>
                    <input
                      type="text"
                      autoComplete="off"
                      value={this.state.email}
                      onChange={this.onEmailChange}
                    />
                  </td>
                </tr>
                <tr className="validation-tr" style={{display: (this.state.emailError) ? '' : 'none'}}>
                  <td></td>
                  <td>
                    <div className="validation-failed-msg">Wrong email format!</div>
                  </td>
                </tr>
                <tr>
                  <td>Language:</td>
                  <td>
                    <select value={this.state.lang} onChange={this.onLangChange}>
                      <option value="sv">Swedish</option>
                      <option value="en">English</option>
                      <option value="de">German</option>
                      <option value="fr">French</option>
                      <option value="nl">Dutch</option>
                      <option value="it">Italian</option>
                      <option value="no">Norwegian</option>
                      <option value="pt">Portuguese</option>
                      <option value="es">Spanish</option>
                      <option value="da">Danish</option>
                      <option value="el">Greek</option>
                      <option value="fi">Finnish</option>
                      <option value="is">Icelandic</option>
                      <option value="et">Estonian</option>
                      <option value="lv">Latvian</option>
                      <option value="lt">Lithuanian</option>
                      <option value="cs">Czech</option>
                      <option value="pl">Polish</option>
                      <option value="hu">Hungarian</option>
                    </select>
                  </td>
                </tr>
                <tr style={{display: (this.props.companyid) ? '' : 'none'}}>
                  <td>Is company admin:</td>
                  <td>
                    <input
                      type="checkbox"
                      autoComplete="off"
                      checked={this.state.isadmin}
                      onClick={this.onIsadminChange}
                    />
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.onClose} />
          <Modal.AcceptButton
            text="Create"
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
