define(["legacy_code", "React", "Backbone", "common/button", "common/uploadbutton", "common/backbone_mixin",
  "signview/tasks/task_mixin"],
  function (legacy_code, React, Backbone, NewButton, UploadButton, BackboneMixin, TaskMixin) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model)
    },

    mixins: [BackboneMixin.BackboneMixin, TaskMixin],

    createTasks: function () {
      var self = this;

      return [new PageTask({
        type: "signatory-attachment",
        isComplete: function () {
          return self.props.model.hasFile();
        },
        el: $(self.refs.uploadArea.getDOMNode()),
        onArrowClick: function () {
          self.refs.uploadButton.openFileDialogue();
        },
        onActivate: function () {
          mixpanel.track("Begin attachment task");
        },
        onDeactivate: function () {
          mixpanel.track("Finish attachment task");
        }
      })];
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    attachmentURL: function () {
      var model = this.props.model;
      var doc = model.document();

      return "/api/frontend/setsignatoryattachment/" + doc.documentid() +
        "/" + doc.viewer().signatoryid() + "/" +
        encodeURIComponent(model.name()) + doc.viewer().urlPart();
    },

    createFileSubmit: function () {
      var self = this;
      var model = self.props.model;

      return new Submit({
        method: "POST",
        url: self.attachmentURL(),
        attachname: model.name(),
        sigattachment: "YES",
        ajax: true,
        expectedType: "json",
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
            ScreenBlockingDialog.open({header: content});
          } else {
            new FlashMessage({content: localization.couldNotUpload, type: "error"});
          }

          model.notLoading();
        },
        ajaxsuccess: function (docdata) {
          var olddocument = model.signatory().document();
          var newdoc = new Document(new Document({
            id: olddocument.documentid(),
            viewer: olddocument.viewer()
          }).parse(docdata));

          _.each(newdoc.currentSignatory().attachments(), function (a) {
            if (a.name() == model.name()) {
              model.setFile(a.file());
            }
          });

          model.notLoading();
        }
      });
    },

    render: function () {
      var self = this;
      var model = self.props.model;
      var doc = model.document();

      var isLoading = model.get("loading");
      var hasFile = model.hasFile();
      var canUpload = doc.pending() || doc.currentSignatoryCanSign();

      return (
        <tr>
          <td className="desc">
            <div className="item">
              {/* if */ hasFile &&
                <div className="filename">
                  <div className="icon" />
                  <div className="label">
                    {model.file().name()}
                  </div>
                </div>
              }
              <div className="name">{model.name()}</div>
              <div className="description">{model.description()}</div>
            </div>
          </td>
          <td className="file">
            <div ref="uploadArea" className="item">
              {/* if */ isLoading && !hasFile &&
                <img className="loading" src="/img/wait30trans.gif" />
              }
              {/* if */ hasFile && !isLoading &&
                <span>
                  <NewButton
                    text={localization.reviewPDF}
                    size="small"
                    className="s-review-sigattachment view-attachment"
                    onClick={function () {
                      window.open(model.file().downloadLink(), "_blank");
                    }}
                  />
                  {/* if */ canUpload &&
                    <NewButton
                      type="cancel"
                      text={localization.deletePDF}
                      className="cancel-attachment"
                      size="small"
                      onClick={function () {
                        model.loading();
                        new Submit({
                          method: "POST",
                          expectedType: "json",
                          url: self.attachmentURL(),
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
                </span>
              }
              {/* if */ canUpload && !isLoading && !hasFile &&
                <UploadButton
                  ref="uploadButton"
                  width={230}
                  size="small"
                  name="file"
                  type="action"
                  className="attachment-upload-button"
                  text={localization.signatoryAttachmentUploadButton}
                  onUploadComplete={function (input) {
                    var submit = self.createFileSubmit();
                    submit.addInputs(input);
                    submit.send();
                  }}
                />
              }
              <div className="clearfix"></div>
            </div>
          </td>
        </tr>
      );
    }
  });
});
