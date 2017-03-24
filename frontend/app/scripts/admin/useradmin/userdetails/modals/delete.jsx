var React = require("react");

var Modal = require("../../../../common/modal");

var DeleteModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onAccept: React.PropTypes.func.isRequired,
    onCancel: React.PropTypes.func.isRequired
  },
  render: function (argument) {
    return (
      <Modal.Container
        className="user-details-modal-delete"
        active={this.props.active}
      >
        <Modal.Header
          title="Delete"
          showClose={true}
          onClose={this.props.onCancel}
        />
        <Modal.Content>
          <div style={{textAlign: "center"}}>
            Are you sure that you want to delete this user?
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onCancel} />
          <Modal.AcceptButton
            text="Delete"
            type="cancel"
            onClick={this.props.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = DeleteModal;
