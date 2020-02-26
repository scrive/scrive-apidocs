var Button = require("../common/button");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var InfoTextInput = require("../common/infotextinput");
var Modal = require("../common/modal");
var NumberValidation = require("../../js/validation.js").NumberValidation;
var Submit = require("../../js/submits.js").Submit;

NumberValidation = new NumberValidation();

var TransferModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onAccept: React.PropTypes.func.isRequired,
    onCancel: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      userid: undefined,
      userNameOrError: ""
    };
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevProps.active == true && prevState.userid != this.state.userid) {
      this.loadUserName();
    }
  },
  componentWillReceiveProps: function (nextProps) {
    if (nextProps.active == true) {
      this.setState(this.getInitialState());
    }
  },
  loadUserName: function () {
    if (NumberValidation.validateData(this.state.userid)) {
      var self = this;

      var submit = new Submit({
        url: "/adminonly/useradmin/details/" + this.state.userid,
        ajaxsuccess: function (resp) {
          var name = (resp.fstname + " " + resp.sndname).trim();
          var email = resp.email;
          var user = name + " (" + email + ")";
          self.setState({userNameOrError: "User: " + user});
        },
        ajaxerror: function () {
          self.setState({
            userNameOrError: "No user matches the given ID"
          });
        }
      });

      submit.sendAjax();
    } else {
      this.setState({
        userNameOrError: "User ID can only contain numbers"
      });
    }
  },
  onUseridChange: function (newUserid) {
    this.setState({userid: newUserid});
  },
  onAcceptButtonClick: function () {
    this.props.onAccept(this.state.userid);
  },
  render: function (argument) {
    var previewStyle = {display: "inline",
                        marginLeft: "20px"};
    return (
      <Modal.Container
        className="dave-transver-modal"
        active={this.props.active}
      >
        <Modal.Header
          title="Transfer template to different user"
          showClose={true}
          onClose={this.props.onCancel}
        />
        <Modal.Content>
          <label>
            User ID:
            <InfoTextInput
              infotext="ID"
              value={this.state.userid}
              onChange={this.onUseridChange}
            />
            <div className="user-name" style={previewStyle}>
              {this.state.userNameOrError}
            </div>
          </label>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onCancel} />
          <Modal.AcceptButton text="Move" onClick={this.onAcceptButtonClick} />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

var TemplateTransfer = React.createClass({
  propTypes: {
    documentid: React.PropTypes.string.isRequired,
  },
  getInitialState: function () {
    return {
      showTransferModal: false
    };
  },
  onTransferButtonClick: function () {
    this.setState({showTransferModal: true});
  },
  onTransferModalClose: function () {
    this.setState({showTransferModal: false});
  },
  onTransfer: function (newUserid) {
    var self = this;
    new Submit({
      url: "/dave/document/" + this.props.documentid + "/transfer",
      method: "POST",
      userid: newUserid,
      ajax: true,
      ajaxsuccess: function () {
        new FlashMessage({type: "success", content: "Transferred"});
        self.setState({showTransferModal: false});
        setTimeout(function () {
          window.location.reload();
        }, 2000);
      },
      ajaxerror: function () {
        new FlashMessage({type: "error", content: "ERROR"});
      },
    }).send();
  },
  render: function () {
    return (
      <div>
        <TransferModal
          active={this.state.showTransferModal}
          onAccept={this.onTransfer}
          onCancel={this.onTransferModalClose}
        />
        <Button
          text="Transfer template to different user"
          onClick={this.onTransferButtonClick}
          type="action"
          />
      </div>
    );
  }
});

module.exports = {"TemplateTransfer": TemplateTransfer};
