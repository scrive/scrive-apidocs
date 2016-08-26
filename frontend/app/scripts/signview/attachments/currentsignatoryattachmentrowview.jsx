var React = require("react");
var Backbone = require("backbone");
var Button = require("../../common/button");
var UploadButton = require("../../common/uploadbutton");
var BackboneMixin = require("../../common/backbone_mixin");
var Track = require("../../common/track");
var TaskMixin = require("../navigation/task_mixin");
var ViewSize = require("../viewsize");
var Submit = require("../../../js/submits.js").Submit;
var $ = require("jquery");
var ReloadManager = require("../../../js/reloadmanager.js").ReloadManager;
var ScreenBlockingDialog = require("../../../js/dialog.js").ScreenBlockingDialog;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Document = require("../../../js/documents.js").Document;
var File = require("../../../js/files.js").File;
var _ = require("underscore");
var Task = require("../navigation/task");

  var UploadArea = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    setAttachmentURL: function () {
      var model = this.props.model;
      var doc = model.document();
      return "/api/frontend/documents/" + doc.documentid() +
        "/" + doc.currentSignatory().signatoryid() + "/setattachment";
    },

    uploadButton: function () {
      return this.refs.uploadButton;
    },

    createFileSubmit: function () {
      var self = this;
      var model = self.props.model;

      return new Submit({
        method: "POST",
        url: self.setAttachmentURL(),
        name: model.name(),
        ajax: true,
        onSend: function () {
          model.loading();
        },
        ajaxerror: function (d, a) {
          if (d && d.status == 409) {
            var button =  new Button({
              type: "optional",
              size: "small",
              text: localization.reloadPage,
              onClick: function () {
                document.location.reload(true);
              }
            });
            var content = $("<div/>");
            content.text(localization.signviewAttachmentUploadedInOtherWindow);
            content.append($("<div style='margin-top: 40px;' />"));
            content.append(button.el());
            ReloadManager.stopBlocking();
            ScreenBlockingDialog.open({header: content});
          } else {
            new FlashMessage({content: localization.couldNotUpload, className: "flash-signview", type: "error"});
          }

          model.notLoading();
        },
        ajaxsuccess: function (docdata) {
          var olddocument = model.signatory().document();
          var newdoc = new Document(new Document({
            id: olddocument.documentid()
          }).parse(docdata));

          _.each(newdoc.currentSignatory().attachments(), function (a) {
            if (a.name() == model.name()) {
              var newFile = a.file();
              if (newFile) {
                model.setFile(
                  new File({
                    id: newFile.fileid(),
                    name: newFile.name(),
                    document: olddocument,
                    documentid: olddocument.documentid()
                  })
                );
              } else {
                model.setFile(undefined);
              }
            }
          });

          model.notLoading();
        }
      });
    },

    render: function () {
      var self = this;
      var model = self.props.model;

      return (
        <div>
          <UploadButton
            ref="uploadButton"
            size="small"
            name="attachment"
            type="action"
            text={localization.signatoryAttachmentUploadButton}
            onError={function () {
              model.notLoading();
              model.trigger("change");
            }}
            onUploadComplete={function (input) {
              var submit = self.createFileSubmit();
              submit.addInputs(input);
              submit.send();
            }}
          />
          <p className="help">{localization.signviewPdfOrPhoto}</p>
        </div>
      );
    }
  });

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    mixins: [BackboneMixin.BackboneMixin, TaskMixin],

    createTasks: function () {
      var self = this;
      var model = self.props.model;
      var uploadArea = self.refs.uploadArea;

      return [new Task({
        type: "signatory-attachment",
        isComplete: function () {
          return !model.get("loading") && model.hasFile();
        },
        pointSelector: ".loader,.button",
        el: $(self.refs["upload-or-load-area"].getDOMNode()),
        onArrowClick: function () {
          uploadArea.uploadButton().openFileDialogue();
        },
        onActivate: function () {
          Track.track("Begin attachment task");
        },
        onDeactivate: function () {
          Track.track("Finish attachment task");
        }
      })];
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    setAttachmentURL: function () {
      var model = this.props.model;
      var doc = model.document();

      return "/api/frontend/documents/" + doc.documentid() +
        "/" + doc.currentSignatory().signatoryid() + "/setattachment";
    },

    render: function () {
      var self = this;
      var model = self.props.model;
      var doc = model.document();

      var isLoading = model.get("loading");
      var hasFile = model.hasFile();
      var canUpload = doc.currentSignatoryCanSign();

      var showUploadArea = canUpload && !hasFile && !isLoading;
      var uploadStyle = {display: showUploadArea ? "inline" : "none"};

      return (
        <div className="section signatory-attachment">
          <div className="col-sm-6 left">
            <h1>
              <span className="paperclip" />
              {model.name()}
            </h1>
            <p className="desc">{model.description()}</p>
          </div>
          <div className="col-sm-6 right" ref="upload-or-load-area">
            {/* if */ isLoading && !hasFile &&
              <div className="loader">
                <div className="loading" />
              </div>
            }
            {/* if */ hasFile && !isLoading &&
              <span>
                <div className={canUpload && "button-group small-buttons"}>
                  {/* if */ ViewSize.isSmall() &&
                    <p className="file-name">{model.file().name()}</p>
                  }
                  <Button
                    text={localization.reviewPDF}
                    className="show-attachment"
                    onClick={function () {
                      window.open(model.file().downloadLink(), "_blank");
                    }}
                  />
                  {/* if */ canUpload &&
                    <Button
                      text={localization.deletePDF}
                      className="transparent-button"
                      onClick={function () {
                        model.loading();
                        new Submit({
                          method: "POST",
                          url: self.setAttachmentURL(),
                          name: model.name(),
                          ajax: true,
                          ajaxerror: function (d, a) {
                            model.notLoading();
                          },
                          ajaxsuccess: function (d) {
                            model.unset("file");
                            model.notLoading();
                          }
                        }).send();
                      }}
                    />
                  }
                </div>
                {/* if */ !ViewSize.isSmall() &&
                  <p className="file-name">{model.file().name()}</p>
                }
              </span>
            }
            <span style={uploadStyle}>
              <UploadArea ref="uploadArea" model={model} />
            </span>
          </div>
        </div>
      );
    }
  });
