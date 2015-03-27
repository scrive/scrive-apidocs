/** @jsx React.DOM */

define(["Underscore", "Backbone", "React", "common/button",
        "designview/typesetters/anchortableview", "legacy_code"],
  function (_, Backbone, React, Button, AnchorTable) {
  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    launchModal: function () {
      var self = this;
      var model = this.props.model;

      var $content = $("<div>");
      var modalContent = React.render(
        <AnchorTable anchors={model.anchors()} />,
        $content[0]
      );

      var popup = new Confirmation({
        title: localization.designview.anchor,
        acceptText: localization.save,
        content: $content,
        onReject: function () {
          React.unmountComponentAtNode($content[0]);
        },
        onAccept: function () {
          self.handleSave(modalContent.getAnchors());
          React.unmountComponentAtNode($content[0]);
          return true;
        }
      });
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
        </div>
      );
    }
  });
});
