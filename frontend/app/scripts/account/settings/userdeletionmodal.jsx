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
    active: React.PropTypes.bool.isRequired
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

  onConfirmation: function (event) {
    if (this.props.onConfirmation && this.correctTextEntered()) {
      return this.props.onConfirmation(event);
    } else {
      return false;
    }
  },

  render: function () {
    var acceptClassName = classNames("user-deletion-button", {
      inactive: !this.correctTextEntered()
    });

    var messageTpl =
          localization.account.accountDetails.userDeletionModalMessage;
    var message = $("<span />").html(messageTpl);
    $(".text", message).text(this.correctText())
      .removeClass("text").addClass("user-deletion-modal-text");

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
          <p dangerouslySetInnerHTML={{__html: message.html()}}></p>
          <input
            type="text"
            value={this.state.textEntered}
            onChange={this.onTextChange}
          />
        </Modal.Content>

        <Modal.Footer>
          <Modal.CancelButton onClick={this.onClose} />
          <Modal.AcceptButton
            className={acceptClassName}
            onClick={this.onConfirmation}
            text={localization.account.accountDetails.deleteUser}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
