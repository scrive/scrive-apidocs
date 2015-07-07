define(["legacy_code", "React", "Backbone", "common/button", "common/uploadbutton",
        "signview/attachments/signatoryattachmentrowview"],
  function (legacy_code, React, Backbone, NewButton, UploadButton, SignatoryAttachmentRow) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    openFileDialogue: function (index) {
      var row = this.refs["attachmentRow-" + index];
      if (row && row.refs.uploadButton) {
        row.refs.uploadButton.openFileDialogue();
      }
    },

    attachmentEls: function () {
      var self = this;
      var doc = self.props.model.document();
      var rows = _.map(doc.currentSignatory().attachments(), function (attachment, i) {
        var row = self.refs["attachmentRow-" + i];
        return row && row.refs.uploadArea && row.refs.uploadArea.getDOMNode();
      });

      return rows;
    },

    componentDidUpdate: function () {
      this.props.model.updateArrowPosition();
    },

    render: function () {
      var model = this.props.model;
      var doc = model.document();
      var hasSigned = doc.currentSignatory().hasSigned();

      var title = hasSigned ? localization.requestedAttachments :
        ((doc.currentSignatory().attachments().length > 1) ?
          localization.docsignview.signatoryAttachmentsTitleForLots :
          localization.docsignview.signatoryAttachmentsTitleForOne);

      var subtitle = localization.docsignview.signatoryAttachmentsSupportedFormats;

      return (
        <div className="signatoryattachments">
          <div className="header">
            <h2>{title}</h2>
            {/* if */ !hasSigned &&
              <div className="subtitle">{subtitle}</div>
            }
          </div>
          <table className="list">
            <tbody>
              {_.map(doc.currentSignatory().attachments(), function (attachment, i) {
                return (
                  <SignatoryAttachmentRow ref={"attachmentRow-" + i} key={i} model={attachment} />
                );
              })}
            </tbody>
          </table>
          <div className="clearfix"></div>
        </div>
      );
    }
  });
});
