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
var classNames = require("classnames");

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

    onUploadComplete: function (fileinput) {
      var self = this;

      _.each(fileinput[0].files, function (file) {
        var originalName = file.name;
        Track.track("Upload attachment", {
          "File Title as supplied by browser": originalName
        });

        var nameParts = originalName.split(".");
        nameParts.pop(); // drop the extension
        var name = nameParts.join(".");

        var newAttachment = new DesignViewAttachment({
          name: name,
          fileUpload: file
        });
        self.props.model.addAttachment(newAttachment);
      });
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
                    multiple={true}
                    onUploadComplete={self.onUploadComplete}
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
                    <td className="attachment-name">{localization.authorattachments.nameOfAttachment}</td>
                    <td className="attachment-shared"></td>
                    <td className="attachment-type">{localization.authorattachments.typeOfAttachment}</td>
                    <td className="attachment-merged">{localization.authorattachments.mergedQuestion}</td>
                    <td className="attachment-view"></td>
                    <td className="attachment-remove"></td>
                  </tr>
                </thead>
                <tbody>
                  {_.map(self.props.model.attachments(), function (a, i) {
                    var trClass = classNames({
                      redborder: a.hasErrorMessage()
                    });

                    return (
                      <tr key={i} className={trClass}>
                        <td>
                          <InfoTextInput
                          value={a.name()}
                          className="atttachment-name"
                          onChange={function (v) {
                            a.setName(v);
                          }}
                          />
                        </td>
                        <td className="attachment-shared">
                          <div
                            className={a.isRequired() ? "required-author-attachment-icon"
                                                      : "optional-author-attachment-icon"}
                          />
                        </td>
                        <td className="attachment-type">
                          <Select
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
                        <td className="attachment-merged">
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
                        <td className="attachment-view">
                          {/* if */ (a.isServerFile()) &&
                            <a
                              className="view-link"
                              href={a.downloadUrl()}
                              target="_blank"
                            />
                          }
                        </td>
                        <td className="attachment-remove">
                          <a
                            className="remove-link"
                            onClick={function () {
                              self.props.model.removeAttachment(a);
                            }}
                          />
                        </td>
                        {/* if */ a.hasErrorMessage() &&
                          <td className="attachment-error">
                            {a.errorMessage()}
                          </td>
                        }
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
