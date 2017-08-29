var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var AttachmentsList = require("./attachmentslist");
var DesignViewAttachment = require("./designviewattachment");
var DesignViewAttachments = require("./designviewattachments");
var Button = require("../../common/button");
var UploadButton = require("../../common/uploadbutton");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var Track = require("../../common/track");
var $ = require("jquery");
var _ = require("underscore");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels: function () {
      return [this.props.model];
    },
    getInitialState: function () {
      return {showAttachmentsList: false};
    },
    isShowingAttachmentsList: function () {
      return this.state.showAttachmentsList;
    },
    showAttachmentsList: function () {
       this.props.onStartShowingList();
       this.setState({showAttachmentsList: true});
    },
    stopShowingAttachmentList: function () {
       this.props.onStopShowingList();
       this.setState({showAttachmentsList: false});
    },
    render: function () {
      var self = this;
      var model = this.props.model;
      return (
        <div className="selectAuthorAttachmentPopupContent">
          { /* if */ (self.state.showAttachmentsList) &&
            (<div>
                <AttachmentsList model={this.props.model} onAdd={self.stopShowingAttachmentList}/>
            </div>)
          }
          { /* else */ (!self.state.showAttachmentsList) &&
            (<div>
              <div className="modal-subtitle centered">
                {localization.selectFiles}
              </div>
              <div className="attachmentsButtonsTable">
                <div style={{width: "45%", display: "inline-block"}}>
                  <UploadButton
                    fileType="application/pdf,.pdf"
                    size="big"
                    text={localization.authorattachments.selectFile}
                    maxlength={2}
                    style={{float: "right"}}
                    onUploadComplete={function (input, title, multifile) {
                      Track.track("Upload attachment", {"File Title as supplied by browser": title});
                      var nameParts = title.split("\\").reverse()[0].split(".");
                      nameParts.pop(); // drop the extension
                      var newAttachment = new DesignViewAttachment({
                        name: nameParts.join("."),
                        fileUpload: $(input)
                      });
                      self.props.model.addAttachment(newAttachment);
                    }}
                  />
                </div>
                <div style={{width: "10%", display: "inline-block"}}/>
                <div  style={{width: "45%", display: "inline-block"}}>
                  <Button
                    size="big"
                    style={{float: "left"}}
                    text={localization.authorattachments.selectAttachment}
                    onClick={function () {
                      Track.track("Click select attachment");
                      self.showAttachmentsList();
                    }}
                  />
                </div>
              </div>
              {/* if */ (!self.props.model.isEmpty()) &&
              (<table className="attachmentsList">
                <thead>
                  <tr>
                    <td style={{width: "210px"}}>{localization.authorattachments.nameOfAttachment}</td>
                    <td style={{width: "25px"}}></td>
                    <td style={{width: "142px"}}>{localization.authorattachments.typeOfAttachment}</td>
                    <td style={{width: "142px"}}>{localization.authorattachments.mergedQuestion}</td>
                    <td style={{width: "28px"}}></td>
                    <td style={{width: "12px"}}></td>
                  </tr>
                </thead>
                <tbody>
                  {_.map(self.props.model.attachments(), function (a, i) {
                    return (
                      <tr key={i}>
                        <td>
                          <InfoTextInput
                          value={a.name()}
                          className="name-input"
                          style={{width: "210px"}}
                          onChange={function (v) {
                            a.setName(v);
                          }}
                          />
                        </td>
                        <td>
                          <div
                            className={a.isRequired() ? "required-author-attachment-icon"
                                                      : "optional-author-attachment-icon"}
                          />
                        </td>
                        <td>
                          <Select
                            width={140}
                            options={[
                              {
                                name: localization.authorattachments.required,
                                selected: a.isRequired(),
                                onSelect: function () { a.makeRequired(); }
                              },
                              {
                                name: localization.authorattachments.optional,
                                selected: !a.isRequired(),
                                onSelect: function () { a.makeOptional(); }
                              }
                            ]}
                          />
                        </td>
                        <td>
                          <Select
                            width={140}
                            options={[
                              {
                                name: localization.authorattachments.merged,
                                selected: a.isAddToSealedFile(),
                                onSelect: function () { a.setAddToSealedFile(true); }
                              },
                              {
                                name: localization.authorattachments.notMerged,
                                selected: !a.isAddToSealedFile(),
                                onSelect: function () { a.setAddToSealedFile(false); }
                              }
                            ]}
                          />
                        </td>
                        <td>
                          {/* if */ (a.isServerFile()) &&
                            <a
                              className="view-link"
                              href={a.downloadUrl()}
                              target="_blank"
                            />
                          }
                        </td>
                        <td>
                          <a
                            className="remove-link"
                            onClick={function () {
                              self.props.model.removeAttachment(a);
                            }}
                          />
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>)
            }
          </div>)
          }
        </div>
      );
    }
  });
