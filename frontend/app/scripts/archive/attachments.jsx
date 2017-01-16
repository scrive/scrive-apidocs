var React = require("react");
var List = require("../lists/list");
var moment = require("moment");
var UploadButton = require("../common/uploadbutton");
var jQuery = require("jquery");
var Submit = require("../../js/submits.js").Submit;
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var $ = require("jquery");

var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Modal = require("../common/modal");

var RemoveModalContent = React.createClass({
  propTypes: {
    attachments: React.PropTypes.array
  },
  render: function () {
    if (this.props.attachments === null) {
      return <span />;
    } else if (this.props.attachments.length == 1) {
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.attachments.remove.body}
          lists={{
            ".put-one-or-more-things-to-be-deleted-here": {
              items: [this.props.attachments[0].field("title")],
              wrapper: "<strong />"
            }
          }}
        />
      );
    } else {
      var sub = this.props.attachments.length + (" " + localization.attachments).toLowerCase();
      return (
        <HtmlTextWithSubstitution
          secureText={localization.archive.attachments.remove.body}
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
        attachmentsToShare: null,
        showRemoveModal: false,
        attachmentsToRemove: null
      };
    },
    attachmentDownloadLink : function(d) {
      return "/a/download/"+ d.field("id") + "/" + d.field("title") +".pdf";
    },
    openShareModal: function(selected) {
      this.setState({
        showShareModal: true,
        attachmentsToShare: _.map(
          selected,
          function (doc) {
            return doc.field("id");
          }
        )
      });
    },
    onShareModalClose: function () {
      this.setState({showShareModal: false, attachmentsToShare: null});
    },
    onShareModalAccept: function () {
      var self = this;

      new Submit({
        url: "/a/share",
        method: "POST",
        attachmentids: "[" + this.state.attachmentsToShare + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: localization.archive.attachments.share.successMessage
          });

          self.reload();
          self.onShareModalClose();
        }
      }).sendAjax();
    },
    openRemoveModal : function(selected) {
      this.setState({
        showRemoveModal: true,
        attachmentsToRemove: selected
      });
    },
    onRemoveModalClose: function () {
      this.setState({showRemoveModal: false, attachmentsToRemove: null});
    },
    onRemoveModalAccept: function () {
      var self = this;
      var attachmentIds = _.map(
        this.state.attachmentsToRemove,
        function (doc) {
          return doc.field("id");
        }
      );

      new Submit({
        url: "/a/delete",
        method: "POST",
        attachmentids: "[" + attachmentIds + "]",
        ajaxsuccess : function() {
          new FlashMessage({
            type: "success",
            content: localization.archive.attachments.remove.successMessage
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
            url='/a'
            dataFetcher={function(d) {return d.attachments;}}
            idFetcher={function(d) {return d.field("id");}}
            paramsFunction = {function(text,_selectfiltering,sorting) {
              var filters = [];
              if (text) {
                filters.push({"filter_by" : "text", "text" : text});
              }
              var sortingBy;
              if (sorting.current()) {
                sortingBy = {
                  sort_by : sorting.current(),
                  order : sorting.isAsc() ? "ascending" : "descending"
                };
              } else {
                sortingBy = {
                  sort_by : "time",
                  order: "descending"
                };
              }
              return {
                filter : JSON.stringify(filters),
                sorting: JSON.stringify([sortingBy])
              };
            }}
            loadLater={self.props.loadLater}
            ref='list'
          >
           <List.TextFiltering text={localization.archive.attachments.search} />

           <List.ListAction
              component={function(model) {
                return (<UploadButton
                  name="doc"
                  text={localization.archive.attachments.createnew.action}
                  className="float-left actionButton"
                  onUploadComplete={function(input) {
                          new Submit({
                          method : "POST",
                          url : "/a",
                          ajaxsuccess : function() {self.reload();},
                          ajaxerror : function() {
                            new FlashMessage({type: 'error', content: localization.couldNotUpload});
                          }
                          }).addInputs($(input)).sendAjax();

                  }}
                />);
              }}
            />

            <List.ListAction
              name={localization.archive.attachments.share.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: 'error', content: localization.archive.attachments.share.emptyMessage});
                  return false;
                }
                self.openShareModal(selected);
              }}
            />

            <List.ListAction
              name={localization.archive.attachments.remove.action}
              onSelect={function(selected,model) {
                if (selected.length ==0 ) {
                  new FlashMessage({type: 'error', content: localization.archive.attachments.remove.emptyMessage});
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
              name={localization.archive.attachments.columns.time}
              width="105px"
              sorting="time"
              rendering={function(d) {
                var time = moment(d.field("time")).toDate();
                return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
              }}
            />
            <List.Column
              name={localization.archive.attachments.columns.attachment}
              width="680px"
              sorting="title"
              rendering={function(d) {
                return (<a target="_blank" href={self.attachmentDownloadLink(d)}>{d.field("title")}</a>);
              }}
            />
            <List.Column
              name={localization.archive.attachments.columns.shared}
              width="75px"
              className="archive-table-shared-column-header"
              rendering={function(d) {
                return (<div className={"archive-table-shared-column " + ((d.field("shared")) ? "sharedIcon" : "")}/>);
              }}
            />

          </List.List>

          <Modal.Container active={self.state.showShareModal}>
            <Modal.Header
              title={localization.archive.attachments.share.head}
              showClose={true}
              onClose={self.onShareModalClose}
            />
            <Modal.Content>
              <p>{localization.archive.attachments.share.body}</p>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onShareModalClose} />
              <Modal.AcceptButton onClick={self.onShareModalAccept} />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={self.state.showRemoveModal}>
            <Modal.Header
              title={localization.archive.attachments.remove.action}
              showClose={true}
              onClose={self.onRemoveModalClose}
            />
            <Modal.Content>
              <RemoveModalContent attachments={self.state.attachmentsToRemove} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onRemoveModalClose} />
              <Modal.AcceptButton type="cancel" onClick={self.onRemoveModalAccept} />
            </Modal.Footer>
          </Modal.Container>
        </div>
      );
    }
});
