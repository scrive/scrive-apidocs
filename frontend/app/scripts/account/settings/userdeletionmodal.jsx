import React from "react";
import Button from "../../common/button";
import Modal from "../../common/modal";
import classNames from "classnames";

module.exports = React.createClass({
  propTypes: {
    onClose: React.PropTypes.func,
    onConfirmation: React.PropTypes.func,
    model: React.PropTypes.object
  },

  getInitialState: function () {
    return {
      correctTextEntered: false
    };
  },

  onTextChange: function (event) {
    console.log(event);
  },

  onClose: function (event) {
    if (this.props.onClose) {
      return this.props.onClose(event);
    } else {
      return false;
    }
  },

  onConfirmation: function (event) {
    if (this.props.onConfirmation && this.state.correctTextEntered) {
      return this.props.onConfirmation(event);
    } else {
      return false;
    }
  },

  render: function () {
    var acceptClassName = classNames({
      disabled: !this.state.correctTextEntered
    });

    return (
      <Modal.Container
        active={true}
        onHide={this.onClose}
      >
        <Modal.Header
          onClose={this.onClose}
          title={localization.account.accountDetails.userDeletionModalTitle}
          showClose={true}
        />

        <Modal.Content>
          <p>TODO: Copy with warning and the text to type in the box.</p>
          <input type="text" onChange={this.onTextChange} />
        </Modal.Content>

        <Modal.Footer>
          <Modal.CancelButton onClick={this.onClose} />
          <Modal.AcceptButton
            className={acceptClassName}
            onClick={this.onConfirmation}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
