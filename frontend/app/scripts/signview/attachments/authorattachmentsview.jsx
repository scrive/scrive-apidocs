define(["legacy_code", "Underscore", "Backbone", "React", "signview/attachments/authorattachmentrow"],
  function (legacy_code, _, Backbone, React, AuthorAttachmentRow) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired
    },

    render: function () {
      var doc = this.props.model;
      var attachments = doc.authorattachments();

      var title = (attachments.length > 1) ?
        localization.docsignview.authorAttachmentsTitleForLots :
        localization.docsignview.authorAttachmentsTitleForOne;

      var canSign = doc.currentSignatoryCanSign();

      var divClass = React.addons.classSet({
        "authorattachments": true,
        "small-screen": BrowserInfo.isSmallScreen()
      });

      return (
        <div className="authorattachments">
          <h2>{title}</h2>
          <table className="list">
            <tbody>
              {_.map(attachments, function (attachment, index) {
                return <AuthorAttachmentRow key={index} canSign={canSign} model={attachment} />;
              })}
            </tbody>
          </table>
          <div className="clearfix"></div>
        </div>
      );
    }
  });
});
