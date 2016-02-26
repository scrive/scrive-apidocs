var React = require("react");
var Backbone = require("backbone");
var SignatoryAttachmentRow = require("./signatoryattachmentrowview");
var Document = require("../../../js/documents.js").Document;
var _ = require("underscore");

  module.exports = React.createClass({
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
