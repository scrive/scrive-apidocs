var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var AuthorAttachmentView = require("./authorattachmentview");
var Document = require("../../../js/documents.js").Document;

  module.exports = React.createClass({
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
