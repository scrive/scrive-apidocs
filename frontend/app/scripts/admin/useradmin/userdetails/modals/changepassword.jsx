var React = require("react");

var InfoTextInput = require("../../../../common/infotextinput");
var Modal = require("../../../../common/modal");
var Submit = require("../../../../../js/submits.js").Submit;

var ChangePasswordModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onAccept: React.PropTypes.func.isRequired,
    onCancel: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      password: ""
    };
  },
  onPasswordChange: function (password) {
    this.setState({password: password});
  },
  onAcceptButtonClick: function () {
    this.props.onAccept(this.state.password);
  },
  render: function (argument) {
    return (
      <Modal.Container
        className="user-details-modal-changepassword"
        active={this.props.active}
      >
        <Modal.Header
          title="Change user password"
          showClose={true}
          onClose={this.props.onCancel}
        />
        <Modal.Content>
          <label>
            Password:
            <InfoTextInput
              infotext="password"
              value={this.state.password}
              onChange={this.onPasswordChange}
            />
          </label>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onCancel} />
          <Modal.AcceptButton text="Change" onClick={this.onAcceptButtonClick} />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = ChangePasswordModal;
