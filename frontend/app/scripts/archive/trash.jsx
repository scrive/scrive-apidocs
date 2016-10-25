var React = require("react");
var Utils = require("./utils");
var StatusTooltipMixin = require("./statustooltipmixin");
var List = require("../lists/list");
var DocumentColumns = require("./document_columns");
var DocumentFilters = require("./document_filters");
var jQuery = require("jquery");
var Submit = require("../../js/submits.js").Submit;
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var Track = require("../common/track");
var $ = require("jquery");

var TimeFilterOptionsMixin = require("./timefilteroptionsmixin");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Modal = require("../common/modal");

var RemoveModalContent = React.createClass({
  propTypes: {
    documents: React.PropTypes.array
  },
  render: function () {
    var message = (localization.archive.trash.remove.body + " " +
                   localization.archive.trash.remove.cannotUndo);
    if (this.props.documents === null) {
      return <p />;
    } else if (this.props.documents.length == 1) {
      return (
        <HtmlTextWithSubstitution
          secureText={message}
          lists={{
            ".put-one-or-more-things-to-be-deleted-here": {
              items: [this.props.documents[0].field("title")],
              wrapper: "<strong />"
            }
          }}
        />
      );
    } else {
      var sub = this.props.documents.length + (" " + localization.documents).toLowerCase();
      return (
        <HtmlTextWithSubstitution
          secureText={message}
          subs={{
            ".put-one-or-more-things-to-be-deleted-here": sub
          }}
        />
      );
    }
  }
});

module.exports = React.createClass({
    mixins: [StatusTooltipMixin, List.ReloadableContainer, TimeFilterOptionsMixin],
    getInitialState: function () {
      return {
        showRestoreModal: false,
        documentsToRestore: null,
        showRemoveModal: false,
        documentsToRemove: null
      };
    },
    openRestoreModal : function(selected) {
      this.setState({
        showRestoreModal: true,
        documentsToRestore: _.map(
          selected, function (doc) {
            return doc.field("id");
          }
        )
      })
    },
    onRestoreModalClose: function () {
      this.setState({
        showRestoreModal: false,
        documentsToRestore: null
      });
    },
    onRestoreModalAccept: function () {
      var self = this;

      new Submit({
        url: "/d/restore",
        method: "POST",
        documentids: "[" + this.state.documentsToRestore + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            color: "green",
            content: localization.archive.trash.restore.successMessage
          });

          self.reload();
          self.onRestoreModalClose();
        }
      }).sendAjax();
    },
    openRemoveModal : function(selected) {
      this.setState({
        showRemoveModal: true,
        documentsToRemove: selected
      });
    },
    onRemoveModalClose: function () {
      this.setState({
        showRemoveModal: false,
        documentsToRemove: null
      });
    },
    onRemoveModalAccept: function () {
      var self = this;
      var documentIds =  _.map(
        this.state.documentsToRemove,
        function (doc) {
          return doc.field("id");
        }
      );

      Track.track('Really delete document');
      new Submit({
        url: "/d/reallydelete",
        method: "POST",
        documentids: "[" + documentIds + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: localization.archive.trash.remove.successMessage
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
                {"filter_by" : "is_in_trash"}
              ])}
            dataFetcher={Utils.dataFetcher}
            idFetcher={Utils.idFetcher}
            loadLater={self.props.loadLater}
            ref='list'
          >
            <List.TextFiltering text={localization.archive.trash.search} />

            <List.ListAction
              name={localization.archive.trash.restore.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: "error", content: localization.archive.trash.restore.emptyMessage});
                  return false;
                }
                self.openRestoreModal(selected);
              }}
            />

            <List.ListAction
              name={localization.archive.trash.remove.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: "error", content: localization.archive.trash.remove.emptyMessage});
                  return false;
                }
                self.openRemoveModal(selected);
              }}
            />

            {DocumentFilters({list: self, fromToFilterOptions: this._fromToFilterOptions})}
            <List.ListSubHeader>
              {localization.archive.trash.subheadline}
            </List.ListSubHeader>
            {DocumentColumns({list:self})}
            <List.Pagination/>
          </List.List>

          <Modal.Container active={this.state.showRestoreModal}>
            <Modal.Header
              title={localization.archive.trash.restore.head}
              showClose={true}
              onClose={this.onRestoreModalClose}
            />
            <Modal.Content>
              <p className="center">{localization.archive.trash.restore.body}</p>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onRestoreModalClose} />
              <Modal.AcceptButton
                text={localization.archive.trash.restore.head}
                onClick={this.onRestoreModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={this.state.showRemoveModal}>
            <Modal.Header
              title={localization.archive.trash.remove.action}
              showClose={true}
              onClose={this.onRemoveModalClose}
            />
            <Modal.Content>
              <RemoveModalContent documents={this.state.documentsToRemove} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onRemoveModalClose} />
              <Modal.AcceptButton
                text={localization.archive.trash.remove.action}
                onClick={this.onRemoveModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>
        </div>
      );
    }
});
