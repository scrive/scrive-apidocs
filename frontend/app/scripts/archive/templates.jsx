var React = require("react");
var Utils = require("./utils");
var List = require("../lists/list");
var moment = require("moment");
var Submit = require("../../js/submits.js").Submit;
var LoadingDialog = require("../../js/loading.js").LoadingDialog;
var jQuery = require("jquery");
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var $ = require("jquery");
var Subscription = require("../account/subscription");
var BlockingModal = require("../blocking/blockingmodal");
var LocationUtils = require("../common/location");
var InfoTextInput = require("../common/infotextinput");
var Document = require("../../js/documents").Document;
var BackboneMixin = require("../common/backbone_mixin");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Modal = require("../common/modal");

var RemoveModalContent = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  propTypes: {
    templates: React.PropTypes.array
  },

  getBackboneModels: function () {
    const tpl = this.state ? this.state.templateForShareableLinkModals : null;
    return tpl ? [tpl] : [];
  },

  render: function () {
    if (this.props.templates === null) {
      return <span />;
    } else if (this.props.templates.length == 1) {
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.templates.remove.body}
          lists={{
            ".put-one-or-more-things-to-be-deleted-here": {
              items: [this.props.templates[0].field("title")],
              wrapper: "<strong />"
            }
          }}
        />
      );
    } else {
      var sub = this.props.templates.length + (" " + localization.templates).toLowerCase();
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.templates.remove.body}
          subs={{
            ".put-one-or-more-things-to-be-deleted-here": sub
          }}
        />
      );
    }
  }
});

  module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    getInitialState: function () {
      return {
        showShareModal: false,
        templatesToShare: null,
        showRemoveModal: false,
        templatesToRemove: null,
        templateForShareableLinkModals: null,
        showShareableLinkModal: false,
        showGenerateShareableLinkModal: false,
        showDiscardShareableLinkModal: false
      };
    },
    createNewTemplate : function() {
      new Submit({
        method : "POST",
        url : "/api/frontend/documents/new",
        ajax: true,
        saved: "true",
        onSend: function() {
          LoadingDialog.open();
        },
        ajaxsuccess: function(doc) {
          doc.is_template = true;
          new Submit({
            method : "POST",
            url : "/api/frontend/documents/"+doc.id+"/update",
            ajax: true,
            document: JSON.stringify(doc),
            ajaxsuccess: function() {
              window.location.href = "/d/"+doc.id;
              LoadingDialog.close();
            }
          }).sendAjax();
        }
      }).sendAjax();
    },
    openShareModal: function(selected) {
      this.setState({
        showShareModal: true,
        templatesToShare: selected
      });
    },
    onShareModalClose: function () {
      this.setState({
        showShareModal: false,
        templatesToShare: null
      });
    },
    onShareModalAccept: function () {
      var self = this;
      var templates = this.state.templatesToShare;
      var templateIds = _.map(
        templates,
        function (doc) {
          return doc.field("id").toString();
        }
      );

      new Submit({
        url: "/api/frontend/documents/templates/setsharing",
        method: "POST",
        document_ids: JSON.stringify(templateIds),
        shared: self.noneAreShared(templates),
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: self.getSharingLocalization(templates).successMessage
          });

          self.reload();
          self.onShareModalClose();
        }
      }).sendAjax();
    },
    openRemoveModal : function(selected) {
      this.setState({
        showRemoveModal: true,
        templatesToRemove: selected
      });
    },
    onRemoveModalClose: function () {
      this.setState({
        showRemoveModal: false,
        templatesToRemove: null
      });
    },
    onRemoveModalAccept: function () {
      var self = this;
      var templateIds = _.map(
        this.state.templatesToRemove,
        function (doc) {
          return doc.field("id");
        }
      );

      new Submit({
        url: "/d/delete",
        method: "POST",
        documentids: "[" + templateIds + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: localization.archive.templates.remove.successMessage
          });

          self.reload();
          self.onRemoveModalClose();
        }
      }).sendAjax();
    },
    getSharingLocalization: function(selected) {
      var shareLoc = {
        action:         localization.archive.templates.share.action,
        head:           localization.archive.templates.share.head,
        body:           localization.archive.templates.share.body,
        successMessage: localization.archive.templates.share.successMessage,
        emptyMessage:   localization.archive.templates.share.emptyMessage
      };
      var unshareLoc = {
        action:         localization.archive.templates.unshare.action,
        head:           localization.archive.templates.unshare.head,
        body:           localization.archive.templates.unshare.body,
        successMessage: localization.archive.templates.unshare.successMessage,
        emptyMessage:   localization.archive.templates.unshare.emptyMessage
      };
      return this.noneAreShared(selected) ? shareLoc : unshareLoc;
    },
    noneAreShared: function(selected) {
      return _.foldl(selected, function(acc, doc) {
        return acc && !doc.field("is_shared");
      }, true);
    },

    openShowShareableLinkModal: function(template) {
      var self = this;
      return function() {
        self.setState({
          templateForShareableLinkModals: template,
          showShareableLinkModal: true
        });
      };
    },

    copyShareableLinkToClipboard: function() {
      var self = this;
      return function() {
        self.selectShareableLink();
        document.execCommand("copy");
        new FlashMessage({
          type: "success",
          content: localization.archive.templates.shareableLink.copySucceeded,
          hideTimeout: 2000
          });
      };
    },

    openGenerateShareableLinkModal: function(template) {
      var self = this;
      return function() {
        self.setState({
          templateForShareableLinkModals: template,
          showGenerateShareableLinkModal: true
        });
      };
    },

    openDiscardShareableLinkModal: function(template) {
      var self = this;
      return function() {
        self.setState({
          templateForShareableLinkModals: template,
          showDiscardShareableLinkModal: true
        });
      };
    },

    closeShowShareableLinkModal: function() {
      this.setState({
        templateForShareableLinkModals: null,
        showShareableLinkModal: false
      });
    },

    closeGenerateShareableLinkModal: function() {
      this.setState({
        templateForShareableLinkModals: null,
        showGenerateShareableLinkModal: false
      });
    },

    closeDiscardShareableLinkModal: function() {
      this.setState({
        templateForShareableLinkModals: null,
        showDiscardShareableLinkModal: false
      });
    },

    selectShareableLink: function() {
      this.refs.input.focus();
      this.refs.input.selectText();
    },

    onGenerateShareableLinkClick: function() {
      const self = this;
      const tpl = this.state.templateForShareableLinkModals;
      const submit = tpl.generateShareableLink(function(resp) {
        self.closeGenerateShareableLinkModal();
        new FlashMessage({
          type: "success",
          content: localization.archive.templates.shareableLink.generationSucceeded
        });
        self.openShowShareableLinkModal(tpl)();
        self.reload();
      }, function(resp) {
        console.log(resp);
        var errorMessage =
          localization.archive.templates.shareableLink.generationFailed;
        new FlashMessage({
          type: "error",
          content: errorMessage
        });
      }).sendAjax();
    },

    onDiscardShareableLinkClick: function() {
      const self = this;
      const tpl = this.state.templateForShareableLinkModals;
      const submit = tpl.discardShareableLink(function(resp) {
        self.closeDiscardShareableLinkModal();
        new FlashMessage({
          type: "success",
          content: localization.archive.templates.shareableLink.discardingSucceeded
        });
        self.reload();
      }, function(resp) {
        new FlashMessage({
          type: "error",
          content: localization.archive.templates.shareableLink.discardingFailed
        });
      }).sendAjax();
    },

    render: function() {
      var self = this;

      const templateShareableLink = this.state.templateForShareableLinkModals
        ? LocationUtils.origin()
            + this.state.templateForShareableLinkModals.shareableLink()
        : "";

      return (
        <div>
          <List.List
            maxPageSize={Utils.maxPageSize}
            totalCountFunction={Utils.totalCountFunction}
            url={Utils.listCallUrl}
            paramsFunction={Utils.paramsFunctionWithFilter([
                {"filter_by" : "is_template"},
                {"filter_by" : "is_author"},
                {"filter_by" : "is_not_in_trash"},
                {"filter_by" : "is_not_part_of_a_flow"}
              ])}
            dataFetcher={Utils.dataFetcher}
            idFetcher={Utils.idFetcher}
            loadLater={self.props.loadLater}
            ref='list'
          >

           <List.TextFiltering text={localization.archive.templates.search} />

            <List.ListAction
              name={localization.archive.templates.createnew}
              locked={!Subscription.currentSubscription().currentUserFeatures().canUseTemplates()}
              onSelect={function() {
                if (!Subscription.currentSubscription().currentUserFeatures().canUseTemplates()) {
                  self.refs.blockingModal.openContactUsModal();
                } else {
                  self.createNewTemplate();
                }
              }}
            />

            <List.ListAction
              makeName={function(selected) {
                return self.getSharingLocalization(selected).action;
              }}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({
                    type: "error",
                    content: self.getSharingLocalization(selected).emptyMessage
                  });
                  return false;
                }
                self.openShareModal(selected);
              }}
            />

            <List.ListAction
              name={localization.archive.templates.remove.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: "error", content: localization.archive.templates.remove.emptyMessage});
                  return false;
                }
                self.openRemoveModal(selected);
              }}
            />

            <List.Column
              select={true}
              width="30px"
            />
            <List.Column
              name={localization.archive.templates.columns.time}
              width="105px"
              sorting="mtime"
              rendering={function(d) {
                var time = moment(d.field("mtime")).toDate();
                // FIXME The fullTime() function doesn't work for some reason,
                // it is located in app/js/utils/time.js and works in scripts/archive/templates.jsx
                return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
              }}
            />
            <List.Column
              name={localization.archive.templates.columns.verificationMethod}
              width="150px"
              rendering={function(d) {
                return (<div>{Utils.documentDeliveryText(d)}</div>);
              }}
            />
           <List.Column
              name={localization.archive.templates.columns.template}
              width="460px"
              sorting="title"
              rendering={function(d) {
                return (<a href={Utils.documentLink(d)}>{d.field("title")}</a>);
              }}
            />
            <List.Column
              name={localization.archive.templates.columns.shared}
              width="75px"
              className="archive-table-shared-column-header"
              rendering={function(d) {
                return (<div className={"archive-table-shared-column " + ((d.field("is_shared")) ? "sharedIcon" : "")}/>);
              }}
            />
            <List.Column
              name={localization.archive.templates.columns.link}
              width="75px"
              className="archive-table-link-column-header"
              rendering={function(r) {
                const d = new Document(r.get("listObjectData"));
                if (Subscription.currentSubscription().currentUserFeatures().canUseShareableLinks()) {
                  if(d.shareableLink()) {
                    return (
                      <div>
                        <div
                          className="shareable-link-icon-show"
                          onClick={self.openShowShareableLinkModal(d)}
                          title={localization.archive.templates.shareableLink.showTooltip}
                        />
                        <div
                          className="shareable-link-icon-regenerate"
                          onClick={self.openGenerateShareableLinkModal(d)}
                          title={localization.archive.templates.shareableLink.regenerateTooltip}
                        />
                        <div
                          className="shareable-link-icon-discard"
                          onClick={self.openDiscardShareableLinkModal(d)}
                          title={localization.archive.templates.shareableLink.discardTooltip}
                        />
                      </div>
                    );
                  } else {
                    return (
                      <div>
                        <div
                          className="shareable-link-icon-generate"
                          onClick={self.openGenerateShareableLinkModal(d)}
                          title={localization.archive.templates.shareableLink.generateTooltip}
                        />
                      </div>
                    );
                  }
                } else {
                  return (
                    <div>
                      <div
                        className="shareable-link-icon-generate-disabled"
                        title={localization.archive.templates.shareableLink.generateTooltip}
                      />
                    </div>
                  );
                }
              }}
            />

            <List.Pagination/>
          </List.List>

          <BlockingModal ref="blockingModal"/>

          <Modal.Container active={self.state.showShareModal}>
            <Modal.Header
              title={this.getSharingLocalization(this.state.templatesToShare).head}
              showClose={true}
              onClose={this.onShareModalClose}
            />
            <Modal.Content>
              <p>{this.getSharingLocalization(this.state.templatesToShare).body}</p>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onShareModalClose} />
              <Modal.AcceptButton onClick={this.onShareModalAccept} />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showRemoveModal}>
            <Modal.Header
              title={localization.archive.templates.remove.action}
              showClose={true}
              onClose={this.onRemoveModalClose}
            />
            <Modal.Content>
              <RemoveModalContent templates={this.state.templatesToRemove} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onRemoveModalClose} />
              <Modal.AcceptButton
                type="cancel"
                text={localization.archive.templates.remove.action}
                onClick={this.onRemoveModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showShareableLinkModal}
                           onClose={this.closeShowShareableLinkModal}
                           marginTop={100}>
            <Modal.Header
              title={localization.archive.templates.shareableLink.showTitle}
              showClose={true}
              onClose={this.closeShowShareableLinkModal}
            />
            <Modal.Content>
              <p>{localization.archive.templates.shareableLink.showText}</p>
              <InfoTextInput
                id="template-shareable-link"
                inputtype="text"
                className="shareable-link-url"
                ref="input"
                readonly={true}
                disabled={false}
                value={templateShareableLink}
                onClick={this.selectShareableLink}
                buttonClassName="shareable-link-icon-copy"
                buttonTooltip={localization.archive.templates.shareableLink.copyButtonTooltip}
                onButtonClick={this.copyShareableLinkToClipboard()}
              />
            </Modal.Content>
            <Modal.Footer>
              <Modal.AcceptButton
                text={localization.ok}
                onClick={this.closeShowShareableLinkModal}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showGenerateShareableLinkModal}
                           onClose={this.closeGenerateShareableLinkModal}>
            <Modal.Header
              title={localization.archive.templates.shareableLink.generateTitle}
              showClose={true}
              onClose={this.closeGenerateShareableLinkModal}
            />
            <Modal.Content>
              {/* if */ this.state.templateForShareableLinkModals &&
                <div>
                  {this.state.templateForShareableLinkModals.shareableLink()
                    ? localization.archive.templates.shareableLink.regenerateText
                    : localization.archive.templates.shareableLink.generateText}
                </div>
              }
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton
                onClick={this.closeGenerateShareableLinkModal} />
              <Modal.AcceptButton
                text={localization.ok}
                onClick={this.onGenerateShareableLinkClick}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showDiscardShareableLinkModal}
                           onClose={this.closeDiscardShareableLinkModal}>
            <Modal.Header
              title={localization.archive.templates.shareableLink.discardTitle}
              showClose={true}
              onClose={this.closeDiscardShareableLinkModal}
            />
            <Modal.Content>
              {localization.archive.templates.shareableLink.discardText}
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton
                onClick={this.closeDiscardShareableLinkModal} />
              <Modal.AcceptButton
                type="cancel"
                text={localization.archive.templates.shareableLink.discardButton}
                onClick={this.onDiscardShareableLinkClick}
              />
            </Modal.Footer>
          </Modal.Container>
        </div>
      );
    }
  });
