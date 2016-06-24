var Backbone = require("backbone");
var React = require("react");
var _ = require("underscore");

var BackboneMixin = require("../common/backbone_mixin").BackboneMixin;
var Button = require("../common/button");
var classNames = require("classnames");
var Document = require("../../js/documents.js").Document;

var EvidenceAttachmentsModel = Backbone.Model.extend({
  defaults: {
     ready: false
  },
  initialize: function (args) {
    this.url = "/api/frontend/documents/" + args.documentId + "/evidenceattachments";
    this.fetch({cache: false});
  },
  ready: function () {
    return this.get("ready");
  },
  attachments: function () {
     return this.get("attachments");
  },
  parse: function (args) {
    return {
      attachments: args.attachments,
      ready: true
    };
  }
});

var AttachmentsTableRowView = React.createClass({
  propTypes: {
    attachment: React.PropTypes.instanceOf(EvidenceAttachmentsModel).isRequired
  },
  onDownloadButtonClick: function (e) {
    window.open(this.props.attachment.download_url, "_blank");
  },
  render: function () {
    return (
      <tr>
        <td className="desc">
          <div className="item">
            <div className="icon"></div>
            <div className="label">
              <div className="name">{this.props.attachment.name}</div>
            </div>
            <div className="clearfix"></div>
          </div>
        </td>
        <td className="file">
          <div className="item">
            <Button
              size="small"
              className="attachment-download-button float-right"
              text={localization.reviewAttachment}
              onClick={this.onDownloadButtonClick}
            />
          </div>
        </td>
      </tr>
    );
  }
});

module.exports = React.createClass({
  mixins: [BackboneMixin],
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  getInitialState: function () {
    return {
      model: new EvidenceAttachmentsModel({
        documentId: this.props.document.documentid()
      })
    };
  },
  getBackboneModels: function () {
    return [this.state.model];
  },
  ready: function () {
    return this.state.model.ready();
  },
  render: function () {
    return (
      <div className="s-evidenceattachments authorattachments">
        <h2>{localization.evidenceAttachmentBoxHeader}</h2>

        <table className="list">
          <tbody>
            {_.map(this.state.model.attachments(), function (item, index) {
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
