var React = require("react");
var _ = require("underscore");

var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var Button = require("../common/button");
var classNames = require("classnames");

var Document = require("../../js/documents.js").Document;
var AuthorAttachment = require("../../js/authorattachment.js").AuthorAttachment;

var AttachmentsTableRowView = React.createClass({
  propTypes: {
    attachment: React.PropTypes.instanceOf(AuthorAttachment).isRequired
  },
  onDownloadButtonClick: function (e) {
    window.open(this.props.attachment.downloadLink(), "_blank");
  },
  render: function () {
    var iconClass = classNames({
      "icon": true,
      "optional": !this.props.attachment.isRequired(),
      "required": this.props.attachment.isRequired()
    });

    var buttonClass = classNames("float-right", "attachment-download-button");

    return (
      <tr>
        <td className="desc">
          <div className="item">
            <div className={iconClass}></div>
            <div className="label">
              <div className="name">{this.props.attachment.name()}</div>
            </div>
            <div className="clearfix"></div>
          </div>
        </td>
        <td className="file">
          <div className="item">
            <Button
              size="small"
              className={buttonClass}
              text={localization.reviewPDF}
              onClick={this.onDownloadButtonClick}
            />
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
    var containerClass = classNames({
      "authorattachments": true,
      "small-screen": BrowserInfo.isSmallScreen()
    });

    return (
      <div className={containerClass}>
        <h2>{localization.authorAttachmentBoxHeader}</h2>

        <table className="list">
          <tbody>
            {_.map(this.props.document.authorattachments(), function (item, index) {
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
