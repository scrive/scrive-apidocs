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

var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Modal = require("../common/modal");

var RemoveModalContent = React.createClass({
  propTypes: {
    templates: React.PropTypes.array
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
        templatesToRemove: null
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
        templatesToShare: _.map(
          selected,
          function (doc) {
            return doc.field("id");
          }
        )
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

      new Submit({
        url: "/d/share",
        method: "POST",
        documentids: "[" + this.state.templatesToShare + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: localization.archive.templates.share.successMessage
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
    render: function() {
      var self = this;
      return (
        <div>
          <List.List
            maxPageSize={Utils.maxPageSize}
            totalCountFunction={Utils.totalCountFunction}
            url={Utils.listCallUrl}
            paramsFunction={Utils.paramsFunctionWithFilter([
                {"filter_by" : "is_template"},
                {"filter_by" : "is_author"},
                {"filter_by" : "is_not_in_trash"}
              ])}
            dataFetcher={Utils.dataFetcher}
            idFetcher={Utils.idFetcher}
            loadLater={self.props.loadLater}
            ref='list'
          >

           <List.TextFiltering text={localization.archive.templates.search} />

            <List.ListAction
              name={localization.archive.templates.createnew}
              locked={!Subscription.currentSubscription().canUseTemplates()}
              onSelect={function() {
                if (!Subscription.currentSubscription().canUseTemplates()) {
                  self.refs.blockingModal.openContactUsModal();
                } else {
                  self.createNewTemplate();
                }
              }}
            />

            <List.ListAction
              name={localization.archive.templates.share.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: "error", content: localization.archive.templates.share.emptyMessage});
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
              width="535px"
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

            <List.Pagination/>
          </List.List>

          <BlockingModal ref="blockingModal"/>

          <Modal.Container active={self.state.showShareModal}>
            <Modal.Header
              title={localization.archive.templates.share.head}
              showClose={true}
              onClose={this.onShareModalClose}
            />
            <Modal.Content>
              <p>{localization.archive.templates.share.body}</p>
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
        </div>
      );
    }
});
