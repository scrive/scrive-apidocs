define(["legacy_code", "React", "Backbone", "signview/attachments/signatoryattachmentrowview"],
  function (legacy_code, React, Backbone, SignatoryAttachmentRow) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
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
        <div className="section spacing">
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
        </div>
      );
    }
  });
});
