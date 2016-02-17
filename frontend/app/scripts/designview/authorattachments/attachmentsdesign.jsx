var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var AttachmentsList = require("./attachmentslist");
var DesignViewAttachment = require("./designviewattachment");
var DesignViewAttachments = require("./designviewattachments");
var Button = require("../../common/button");
var UploadButton = require("../../common/uploadbutton");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
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
                <div style={{width:"45%", display: "inline-block"}}>
                  <UploadButton
                    size="big"
                    text={localization.authorattachments.selectFile}
                    maxlength={2}
                    style={{float: "right"}}
                    onUploadComplete={function (input, title, multifile) {
                      mixpanel.track("Upload attachment", {"File Title as supplied by browser": title});
                      var name_parts = title.split("\\").reverse()[0].split(".");
                      name_parts.pop(); // drop the extension
                      var newAttachment = new DesignViewAttachment({
                        name: name_parts.join("."),
                        fileUpload: $(input)
                      });
                      self.props.model.addAttachment(newAttachment);
                    }}
                  />
                </div>
                <div style={{width:"10%", display: "inline-block"}}/>
                <div  style={{width:"45%", display: "inline-block"}}>
                  <Button
                    size="big"
                    style={{float: "left"}}
                    text={localization.authorattachments.selectAttachment}
                    onClick={function () {
                      mixpanel.track("Click select attachment");
                      self.showAttachmentsList();
                    }}
                  />
                </div>
              </div>
              {/* if */ (!self.props.model.isEmpty()) &&
              (<table className="attachmentsList" style={{borderCollapse: "separate", borderSpacing: "5px"}}>
                <thead>
                  <tr>
                    <td style={{width:"25px"}}></td>
                    <td style={{width:"330px"}}>{localization.authorattachments.nameOfAttachment}</td>
                    <td colSpan="3" style={{width:"242px"}}>{localization.authorattachments.typeOfAttachment}</td>
                  </tr>
                </thead>
                <tbody>
                  {_.map(self.props.model.attachments(), function (a, i) {
                    return (
                      <tr key={i}>
                        <td>
                          <div
                            className={a.isRequired() ? "required-author-attachment-icon"
                                                      : "optional-author-attachment-icon"}
                          />
                        </td>
                        <td>
                          <InfoTextInput
                          value={a.name()}
                          className="name-input"
                          onChange={function (v) {
                            a.setName(v);
                          }}
                          />
                        </td>
                        <td>
                          <Select
                            name={ a.isRequired() ? localization.authorattachments.required
                                                  : localization.authorattachments.optional }
                            options={[
                              {
                                name: localization.authorattachments.required,
                                onSelect: function () { a.makeRequired();}
                              },
                              {
                                name: localization.authorattachments.optional,
                                onSelect: function () { a.makeOptional();}
                              }
                            ]}
                          />
                        </td>
                        <td style={{width:"48px"}}>
                          {/* if */ (a.isServerFile()) &&
                            <a
                              className="view-link"
                              href={a.downloadUrl()}
                              target="_blank"
                            />
                          }
                        </td>
                        <td style={{width:"25px"}}>
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
