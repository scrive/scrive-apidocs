var React = require("react");
var Utils = require("./utils");
var List = require("../lists/list");
var moment = require("moment");
var Submit = require("../../js/submits.js").Submit;
var LoadingDialog = require("../../js/loading.js").LoadingDialog;
var Confirmation = require("../../js/confirmations.js").Confirmation;
var jQuery = require("jquery");
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var $ = require("jquery");



module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
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
      var self = this;
      var confirmationPopup = new Confirmation({
          acceptText: localization.ok,
          rejectText: localization.cancel,
          title: localization.archive.templates.share.head,
          content: jQuery("<p/>").text(localization.archive.templates.share.body),
          onAccept : function() {
            new Submit({
              url: "/d/share",
              method: "POST",
              documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
              ajaxsuccess : function() {
                new FlashMessage({type: "success", content : localization.archive.templates.share.successMessage});
                self.reload();
                confirmationPopup.clear();
              }
            }).sendAjax();
          }
      });
      return true;
    },
    openRemoveModal : function(selected) {
      var self = this;
      var confirmationText = $('<span />').html(localization.archive.templates.remove.body);
      var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
      if (selected.length == 1) {
        listElement.html($('<strong />').text(selected[0].field("title")));
      } else {
        listElement.text(selected.length + (" " + localization.templates).toLowerCase());
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.templates.remove.action,
        rejectText: localization.cancel,
        title: localization.archive.templates.remove.action,
        content: confirmationText,
        oneClick: true,
        onAccept : function() {
          new Submit({
            url: "/d/delete",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type: "success", content : localization.archive.templates.remove.successMessage});
              self.reload();
              confirmationPopup.clear();
            }
          }).sendAjax();
        }
      });
      return true;
    },
    render: function() {
      var self = this;
      return (
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
            onSelect={function() {
              self.createNewTemplate();
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
      );
    }
});
