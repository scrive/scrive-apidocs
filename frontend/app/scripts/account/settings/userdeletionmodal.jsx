import React from "react";
import Button from "../../common/button";
import Modal from "../../common/modal";
import classNames from "classnames";
import $ from "jquery";

module.exports = React.createClass({
  propTypes: {
    onClose: React.PropTypes.func.isRequired,
    onConfirmation: React.PropTypes.func.isRequired,
    model: React.PropTypes.object.isRequired,
    active: React.PropTypes.bool.isRequired,
    lastCompanyUser: React.PropTypes.bool.isRequired
  },

  getInitialState: function () {
    return {
      textEntered: ""
    };
  },

  componentWillReceiveProps: function (nextProps) {
    if (!this.props.active && nextProps.active) {
      this.setState({textEntered: ""});
    }
  },

  onTextChange: function (event) {
    this.setState({textEntered: event.target.value});
  },

  correctText: function () {
    return this.props.model.email();
  },

  correctTextEntered: function () {
    return this.state.textEntered == this.correctText();
  },

  onClose: function (event) {
    if (this.props.onClose) {
      return this.props.onClose(event);
    } else {
      return false;
    }
  },

  onConfirmation: function () {
    if (this.props.onConfirmation && this.correctTextEntered()) {
      return this.props.onConfirmation(this.state.textEntered);
    } else {
      return false;
    }
  },

  render: function () {
    var acceptClassName = classNames("user-deletion-button", {
      inactive: !this.correctTextEntered()
    });

    var messageTpl =
          localization.account.accountDetails.userDeletionModalMessage.line4;
    var message = $("<span />").html(messageTpl);
    $(".email", message).text(this.correctText())
      .removeClass("text").addClass("user-deletion-modal-text");

    var line1 = this.props.lastCompanyUser
      ? localization.account.accountDetails.userDeletionModalMessage.line1last
      : localization.account.accountDetails.userDeletionModalMessage.line1;

    return (
      <Modal.Container
        active={this.props.active}
        onHide={this.onClose}
        onClose={this.onClose}
      >
        <Modal.Header
          onClose={this.onClose}
          title={localization.account.accountDetails.userDeletionModalTitle}
          showClose={true}
        />

        <Modal.Content>
          <div>
            <p>
              {line1}
            </p>
            <p>
              {localization.account.accountDetails.userDeletionModalMessage.line2}
            </p>
            <p>
              <strong>
                {localization.account.accountDetails.userDeletionModalMessage.line3}
              </strong>
            </p>
            <p dangerouslySetInnerHTML={{__html: message.html()}}></p>
          </div>
          <input
            type="text"
            value={this.state.textEntered}
            onChange={this.onTextChange}
          />
        </Modal.Content>

        <Modal.Footer>
          <Modal.CancelButton onClick={this.onClose} />
          <Modal.AcceptButton
            type="cancel"
            className={acceptClassName}
            onClick={this.onConfirmation}
            text={localization.account.accountDetails.userDeletionModalConfirm}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
