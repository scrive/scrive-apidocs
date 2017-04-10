var React = require("react");

var Button = require("../../../common/button");
var MergeModal = require("./modals/merge");

var ButtonBarView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    companyId: React.PropTypes.string.isRequired,
    onMerge: React.PropTypes.func.isRequired,
    onSave: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      showMergeModal: false
    };
  },
  onMergeButtonClick: function () {
    this.setState({showMergeModal: true});
  },
  onMergeModalCancel: function () {
    this.setState({showMergeModal: false});
  },
  render: function () {
    return (
      <div className="company-details-button-bar">
        <Button
          className="company-details-button-merge"
          text="Merge to different company"
          type="optional"
          size="tiny"
          onClick={this.onMergeButtonClick}
        />

        <Button
          className="company-details-button-save"
          text="Change details"
          type="action"
          size="tiny"
          onClick={this.props.onSave}
        />

        <MergeModal
          active={this.state.showMergeModal}
          companyId={this.props.companyId}
          onAccept={this.props.onMerge}
          onCancel={this.onMergeModalCancel}
        />
      </div>
    );
  }
});

module.exports = ButtonBarView;
