define(["legacy_code", "Underscore", "Backbone", "React", "signview/attachments/authorattachmentview"],
  function (legacy_code, _, Backbone, React, AuthorAttachmentView) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired,
      canStartFetching: React.PropTypes.bool.isRequired
    },

    render: function () {
      var self = this;
      var doc = this.props.model;
      return (
        <div>
        {_.map(doc.authorattachments(), function (a, i) {
          return (
             <AuthorAttachmentView
               key={i}
               model={a}
               canSign={doc.currentSignatoryCanSign()}
               canStartFetching={self.props.canStartFetching}
             />
          );
        })}
        </div>
      );
    }
  });
});
