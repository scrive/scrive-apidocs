var React = require("react");
var _ = require("underscore");

var Button = require("../common/button");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Document = require("../../js/documents.js").Document;
var SignatoryAttachment = require("../../js/signatoryattachment.js").SignatoryAttachment;
var classNames = require("classnames");

var AttachmentsTableRowView = React.createClass({
  propTypes: {
    attachment: React.PropTypes.instanceOf(SignatoryAttachment).isRequired
  },
  onDownloadButtonClick: function (e) {
    window.open(this.props.attachment.file().downloadLink(), "_blank");
  },
  attachmentText: function () {
    var attachment = this.props.attachment;
    if (attachment.hasFile()) {
      return localization.authorview.uploadedBy;
    } else if (attachment.isRequired()) {
        return localization.authorview.requestedFrom;
    } else if (attachment.signatory().hasSigned()) {
        return localization.authorview.notUploaded;
    } else {
        return localization.authorview.optionallyRequestedFrom;
    }
  },
  render: function () {
    var attachment = this.props.attachment;
    var iconClass = classNames({
      "icon": true,
      "optional": !this.props.attachment.isRequired(),
      "required": this.props.attachment.isRequired()
    });

    return (
      <tr>
        <td className="desc">
          <div className="item">
            <div className={iconClass}></div>
            <div className="label">
              <HtmlTextWithSubstitution
                secureText={this.attachmentText()}
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
