var React = require("react");
var _ = require("underscore");

var SignatoryAttachmentRow = require("./signatoryattachmentrow");

var SignatoryAttachmentsTable = React.createClass({
  propTypes: {
    attachments: React.PropTypes.array.isRequired,
    signatories: React.PropTypes.array.isRequired,
    onRemove: React.PropTypes.func.isRequired
  },
  render: function () {
    var self = this;

    return (
      <table className="editSignatoryAttachmentTable">
        <thead>
          <tr>
            <th className="editSignatoryAttachmentTDName">
              {localization.signatoryAttachments.attachment}
            </th>
            <th className="editSignatoryAttachmentTDDescription">
              {localization.signatoryAttachments.description}
            </th>
            <th className="editSignatoryAttachmentTDIcon" />
            <th className="editSignatoryAttachmentTDSelect">
              {localization.signatoryAttachments.typeOfAttachment}
            </th>
            <th className="editSignatoryAttachmentTDSelect">
              {localization.signatoryAttachments.from}
            </th>
            <th className="editSignatoryAttachmentTDIcon" />
          </tr>
        </thead>

        <tbody>
          {_.map(this.props.attachments, function (attachment) {
            return (
              <SignatoryAttachmentRow
                key={attachment.cid}
                attachment={attachment}
                signatories={self.props.signatories}
                onRemove={self.props.onRemove}
              />
            );
          })}
        </tbody>
      </table>
    );
  }
});

module.exports = SignatoryAttachmentsTable;
