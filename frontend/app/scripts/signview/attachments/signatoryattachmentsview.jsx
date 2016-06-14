var React = require("react");
var Backbone = require("backbone");
var CurrentSignatoryAttachmentRow = require("./currentsignatoryattachmentrowview");
var OtherSignatoryAttachmentRow = require("./othersignatoryattachmentrowview");
var Document = require("../../../js/documents.js").Document;
var _ = require("underscore");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired
    },

    render: function () {
      var doc = this.props.model;

      var othersignatories = _.filter(doc.signatories(), function (s) {
        return s.signs() && !s.current() && s.hasSigned();
      });

      var othersignatoriesattachments = _.flatten(_.map(othersignatories, function (s) {
        return s.attachments();
      }));

      var othersignatoriesuploadedattachments = _.filter(othersignatoriesattachments, function (a) {
        return a.hasFile();
      });

      return (
        <span>
          {_.map(doc.currentSignatory().attachments(), function (attachment, i) {
            return (
              <CurrentSignatoryAttachmentRow
                key={i}
                model={attachment}
              />
            );
          })}

          {_.map(othersignatoriesuploadedattachments, function (attachment, i) {
            return (
              <OtherSignatoryAttachmentRow
                key={i}
                model={attachment}
              />
            );
          })}
        </span>
      );
    }
  });
