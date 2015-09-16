define([
  "legacy_code",
  "Backbone",
  "React",
  "common/retargeting_service",
  "common/backbone_mixin",
  "signview/create_account_section_view",
  "signview/signatories/docviewsignatories",
  "signview/attachments/signatoryattachmentsview",
  "signview/instructionsview/instructionsview",
  "signview/attachments/authorattachmentsview",
  "signview/extradetails/extradetailsview",
  "signview/signsection/signsectionview",
  "signview/signviewmodel",
  "signview/fileview/fileview",
  "signview/header",
  "signview/footer",
  "signview/tasks/taskarrows"
], function (
  legacy_code,
  Backbone,
  React,
  RetargetingService,
  BackboneMixin,
  CreateAccountSection,
  DocumentViewSignatories,
  SignatoryAttachmentsView,
  InstructionsView,
  AuthorAttachmentsView,
  ExtraDetailsView,
  SignSectionView,
  SignViewModel,
  FileView,
  Header,
  Footer,
  TaskArrows
) {
  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      documentId: React.PropTypes.string.isRequired,
      sigLinkId: React.PropTypes.string.isRequired,
      allowSaveSafetyCopy: React.PropTypes.bool.isRequired,
      useStandardHeaders: React.PropTypes.bool.isRequired,
      authorFullname: React.PropTypes.string,
      authorPhone: React.PropTypes.string,
      link: React.PropTypes.object,
      forceShowing: React.PropTypes.bool
    },

    getInitialState: function () {
      var viewer = new DocumentViewer({
        signatoryid: this.props.sigLinkId
      });

      var model = new SignViewModel({
        document: new Document({id: this.props.documentId, viewer: viewer}),
        allowsavesafetycopy: this.props.allowSaveSafetyCopy
      });

      return {model: model};
    },

    childContextTypes: {
      addTask: React.PropTypes.func.isRequired,
      removeTask: React.PropTypes.func.isRequired
    },

    getChildContext: function () {
      var model = this.state.model;

      return {
        addTask: function (task) {
          model.addTask(task);
        },

        removeTask: function (task) {
          model.removeTask(task);
        }
      };
    },

    componentDidMount: function () {
      this.state.model.recall();
      ReloadManager.pushBlock(this.state.model.blockReload);
    },

    getBackboneModels: function () {
      return [this.state.model, this.state.model.document()];
    },

    render: function () {
      var model = this.state.model;
      var doc = model.document();
      var arrow = function () {
        return model.arrow();
      };

      return (
        <div className="signview">
          {/* if */ !this.props.useStandardHeaders &&
            <Header
              document={doc}
              documentid={this.props.documentId}
              link={this.props.link}
              forceShowing={this.props.forceShowing}
              signatorylinkid={this.props.sigLinkId}
              authorFullname={this.props.authorFullname}
              authorPhone={this.props.authorPhone}
            />
          }
          {/* if */ model.hasArrows() &&
            <TaskArrows model={model} />
          }
          <div className="mainContainer">
            {/* if */ !model.isReady() &&
              <div className="subcontainerWrapper">
                <div className="subcontainer">
                  <br/>
                  <div className="document-pages">
                    <div className="waiting4page" />
                  </div>
                </div>
              </div>
            }
            {/* else */ model.isReady() &&
              <div className="subcontainerWrapper">
                <InstructionsView model={model} />
                <div className="subcontainer">
                  {/* if */ model.hasCreateAccountSection() &&
                    <CreateAccountSection document={doc} />
                  }
                  {/* if */ model.hasMainFileSection() &&
                    <FileView model={doc.mainfile()} signview={model} arrow={arrow} />
                  }
                  {/* if */ model.hasAuthorAttachmentsSection() &&
                    <AuthorAttachmentsView model={doc} />
                  }
                  {/* if */ model.hasExtraDetailsSection() &&
                    <ExtraDetailsView model={doc.currentSignatory()} signview={model} />
                  }
                  {/* if */ model.hasSignatoriesAttachmentsSection() &&
                    <SignatoryAttachmentsView model={model} />
                  }
                  {/* if */ model.hasSignatoriesSection() &&
                    <DocumentViewSignatories document={doc} />
                  }
                  {/* if */ model.hasSignSection() &&
                    <SignSectionView
                      model={model}
                      onMount={function () {
                        model.sendTrackingData();
                        model.takeFirstScreenshotWithDelay();
                      }}
                    />
                  }
                  {/* if */ model.hasAnySection() && <div className="clearfix" />}
                </div>
              </div>
            }
            <div className="clearfix" />
            <div className="spacer40" />
          </div>
          {/* if */ !this.props.useStandardHeaders &&
            <Footer document={doc} />
          }
        </div>
      );
    }
  });
});
