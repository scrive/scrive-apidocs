var React = require("react");

var Button = require("../../../common/button");
var DeleteModal = require("./modals/delete");
var MoveModal = require("./modals/move");

var ButtonBarView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    companyid: React.PropTypes.string.isRequired,
    onDelete: React.PropTypes.func.isRequired,
    onResendInvitation: React.PropTypes.func.isRequired,
    onMove: React.PropTypes.func.isRequired,
    onSave: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      showDeleteModal: false,
      showMoveModal: false
    };
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (this.state.showMoveModal && prevProps.companyid != this.props.companyid) {
      this.setState({showMoveModal: false});
    }
  },
  onDeleteButtonClick: function () {
    this.setState({showDeleteModal: true});
  },
  onDeleteModalCancel: function () {
    this.setState({showDeleteModal: false});
  },
  onMoveButtonClick: function () {
    this.setState({showMoveModal: true});
  },
  onMoveModalCancel: function () {
    this.setState({showMoveModal: false});
  },
  render: function () {
    return (
      <div className="user-details-button-bar">
        <Button
          className="user-details-button-delete"
          text="Delete user"
          type="cancel"
          size="tiny"
          onClick={this.onDeleteButtonClick}
        />

        <Button
          className="user-details-button-resend-invitation"
          text="Resend invitation"
          type="optional"
          size="tiny"
          onClick={this.props.onResendInvitation}
        />

        <Button
          className="user-details-button-move"
          text="Move to different company"
          type="optional"
          size="tiny"
          onClick={this.onMoveButtonClick}
        />

        <Button
          className="user-details-button-save"
          text="Change details"
          type="action"
          size="tiny"
          onClick={this.props.onSave}
        />

        <DeleteModal
          active={this.state.showDeleteModal}
          onAccept={this.props.onDelete}
          onCancel={this.onDeleteModalCancel}
        />

        <MoveModal
          active={this.state.showMoveModal}
          companyid={this.props.companyid}
          onAccept={this.props.onMove}
          onCancel={this.onMoveModalCancel}
        />
      </div>
    );
  }
});

module.exports = ButtonBarView;
