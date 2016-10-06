var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var AnchorTable = require("./anchortableview");
var $ = require("jquery");

var Modal = require("../../common/modal");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    getInitialState: function () {
      return {
        showAnchorModal: false
      };
    },

    launchModal: function () {
      this.setState({showAnchorModal: true});
    },

    onAnchorModalClose: function () {
      this.setState({showAnchorModal: false});
    },

    onAnchorModalAccept: function () {
      this.handleSave(this.refs.anchorTable.getAnchors());
      this.onAnchorModalClose();
    },

    formatAnchor: function (anchor) {
      return anchor.text() + ", " + anchor.index();
    },

    handleSave: function (anchors) {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var doc = sig.document();

      var validAnchors = _.filter(anchors, function (anchor) {
        return anchor.isValid();
      });

      model.setAnchors(validAnchors);
    },

    render: function () {
      var model = this.props.model;
      var anchors = model.anchors();
      var buttonText = anchors.length > 0 ?
        localization.designview.editField : localization.designview.addAnchor;

      return (
        <div className="subtitle">
          {localization.designview.anchor}
          <div className="fieldTypeSetter-subtitle-select">
            {anchors.length > 0 &&
            <span className="fieldTypeSetter-subtitle-anchor">
              {_.map(anchors, this.formatAnchor).join(". ")}
            </span>
            }
            <Button
              size="tiny"
              text={buttonText}
              onClick={this.launchModal}
            />
          </div>

          <Modal.Container active={this.state.showAnchorModal}>
            <Modal.Header
              title={localization.designview.anchor}
              showClose={true}
              onClose={this.onAnchorModalClose}
            />
            <Modal.Content>
              <AnchorTable ref="anchorTable" anchors={model.anchors()} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onAnchorModalClose} />
              <Modal.AcceptButton
                text={localization.save}
                onClick={this.onAnchorModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>
        </div>
      );
    }
  });
