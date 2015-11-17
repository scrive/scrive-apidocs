define(["legacy_code", "React", "Backbone", "signview/attachments/signatoryattachmentrowview"],
  function (legacy_code, React, Backbone, SignatoryAttachmentRow) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired
    },

    render: function () {
      var doc = this.props.model;

      return (
        <span>
          {_.map(doc.currentSignatory().attachments(), function (attachment, i) {
            return (
              <SignatoryAttachmentRow ref={"attachmentRow-" + i} key={i} model={attachment} />
            );
          })}
        </span>
      );
    }
  });
});
