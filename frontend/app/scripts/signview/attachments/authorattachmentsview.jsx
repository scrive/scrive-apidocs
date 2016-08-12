var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var AuthorAttachmentView = require("./authorattachmentview");
var Document = require("../../../js/documents.js").Document;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired,
      canStartFetching: React.PropTypes.bool.isRequired,
      showOverlay: React.PropTypes.bool.isRequired,
      showArrow: React.PropTypes.bool.isRequired
    },

    attachmentsToShow: function () {
      var doc = this.props.model;
      doc.authorattachments();
      return _.filter(doc.authorattachments(), function (a) {
        return !doc.isSignedAndClosed()  || !a.isAddToSealedFile();
      });
    },

    render: function () {
      var self = this;
      var doc = this.props.model;
      return (
        <div>
        {_.map(this.attachmentsToShow(), (a, i) => {
          return (
             <AuthorAttachmentView
               ref={"attachment-view-" + i}
               key={i}
               model={a}
               canSign={doc.currentSignatoryCanSign()}
               canStartFetching={self.props.canStartFetching}
               showOverlay={this.props.showOverlay}
               showArrow={this.props.showArrow}
             />
          );
        })}
        </div>
      );
    }
  });
