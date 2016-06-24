var React = require("react");
var _ = require("underscore");

var Button = require("../common/button");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Document = require("../../js/documents.js").Document;
var SignatoryAttachment = require("../../js/signatoryattachment.js").SignatoryAttachment;

var AttachmentsTableRowView = React.createClass({
  propTypes: {
    attachment: React.PropTypes.instanceOf(SignatoryAttachment).isRequired
  },
  onDownloadButtonClick: function (e) {
    window.open(this.props.attachment.file().downloadLink(), "_blank");
  },
  render: function () {
    var attachment = this.props.attachment;

    return (
      <tr>
        <td className="desc">
          <div className="item">
            <div className="label">
              <HtmlTextWithSubstitution
                secureText={
                  (attachment.hasFile())
                    ? localization.authorview.uploadedBy
                    : localization.authorview.requestedFrom
                }
                subs={{
                  ".put-attachment-name-here": attachment.name(),
                  ".put-signatory-name-here": attachment.signatory().nameOrEmail()
                }}
              />
              <div className="description">"{this.props.attachment.description()}"</div>
            </div>
            <div className="clearfix"></div>
          </div>
        </td>
        <td className="file">
          <div className="item">
            {/* if */ (this.props.attachment.hasFile()) &&
              <Button
                size="small"
                className="float-right attachment-download-button"
                text={localization.reviewPDF}
                onClick={this.onDownloadButtonClick}
              />
            }
          </div>
        </td>
      </tr>
    );
  }
});

module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  render: function () {
    return (
      <div className="signatoryattachments">
        <h2>{localization.authorview.requestedAttachments}</h2>

        <table className="list">
          <tbody>
            {_.map(this.props.document.signatoryattachments(), function (item, index) {
              return <AttachmentsTableRowView
                key={index}
                attachment={item}
              />;
            })}
          </tbody>
        </table>
      </div>
    );
  }
});
