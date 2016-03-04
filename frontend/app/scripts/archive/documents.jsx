var React = require("react");
var Utils = require("./utils");
var StatusTooltipMixin = require("./statustooltipmixin");
var List = require("../lists/list");
var DocumentColumns = require("./document_columns");
var DocumentFilters = require("./document_filters");
var jQuery = require("jquery");
var $ = require("jquery");
var Confirmation = require("../../js/confirmations.js").Confirmation;
var Submit = require("../../js/submits.js").Submit;
var _ = require("underscore");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;


module.exports = React.createClass({
    mixins: [StatusTooltipMixin,List.ReloadableContainer],
    openSendReminderModal : function(selected) {
      var self = this;
      var content = jQuery("<p/>");
      if (selected.length == 1) {
        var span = $('<span />').html(localization.archive.documents.sendreminder.bodysingle);
        span.find('.put-document-name-here').html(jQuery("<strong/>").text(selected[0].field("title")));
        content.append(span);
      } else {
        var span = $('<span />').html(localization.archive.documents.sendreminder.bodymulti);
        span.find('.put-number-of-documents-here').text(selected.length);
        content.append(span);
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.ok,
        rejectText: localization.cancel,
        title: localization.archive.documents.sendreminder.action,
        content: content,
        onAccept : function() {
          mixpanel.track('Send reminder');
          new Submit({
            url: "/d/remind",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type: 'success', content : localization.archive.documents.sendreminder.successMessage});
              self.reload();
              confirmationPopup.clear();
            },
            ajaxerror : function() {
              new FlashMessage({type: 'error', content : localization.archive.documents.sendreminder.errorMessage});
              self.reload();
              confirmationPopup.clear();
            }
          }).sendAjax();
        }
      });
      return true;
    },
    openCancelModal : function(selected) {
      var self = this;
      var confirmationPopup = new Confirmation({
        acceptText: localization.ok,
        rejectText: localization.cancel,
        title: localization.archive.documents.cancel.action,
        content: jQuery("<p/>").text(localization.archive.documents.cancel.body),
        onAccept : function() {
          mixpanel.track('Cancel document');
          new Submit({
            url: "/d/cancel",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type: 'success', content : localization.archive.documents.cancel.successMessage});
              self.reload();
              confirmationPopup.clear();
            },
            ajaxerror : function() {
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
      var confirmationText = $('<span />').html(localization.archive.documents.remove.body);
      var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
      if (selected.length == 1) {
        listElement.html($('<strong />').text(selected[0].field("title")));
      } else {
        listElement.text(selected.length + (" " + localization.documents).toLowerCase());
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.documents.remove.action,
        rejectText: localization.cancel,
        title: localization.archive.documents.remove.action,
        content: confirmationText,
        oneClick: true,
        onAccept : function() {
          mixpanel.track('Delete document');
          new Submit({
            url: "/d/delete",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type: 'success', content : localization.archive.documents.remove.successMessage});
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
      var idleRemark;
      if (self.props.idledoctimeout == 1) {
        idleRemark = localization.archive.documents.idleRemark1;
      } else {
        idleRemark = $("<div/>").html(localization.archive.documents.idleRemark);
        idleRemark.find('.put-idledoctimeout-here').text(self.props.idledoctimeout);
        idleRemark = idleRemark.text();
      }
      return (
        <List.List
          maxPageSize={Utils.maxPageSize}
          totalCountFunction={Utils.totalCountFunction}
          url={Utils.listCallUrl}
          paramsFunction={Utils.paramsFunctionWithFilter([
              {"filter_by" : "is_not_template"},
              {"filter_by" : "is_not_in_trash"}
            ])}
          dataFetcher={Utils.dataFetcher}
          idFetcher={Utils.idFetcher}
          loadLater={self.props.loadLater}
          ref='list'
        >
          <List.TextFiltering text={localization.archive.documents.search} />

          <List.ListAction
            name={localization.archive.documents.sendreminder.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({type: 'error', content: localization.archive.documents.sendreminder.emptyMessage});
              } else {
                var allSelectedArePending = _.all(selected, function(doc) {
                      return doc.field("status") == "pending";
                });
                if (!allSelectedArePending) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.sendreminder.notAvailableMessage});
                } else {
                  self.openSendReminderModal(selected);
                }
              }
            }}
          />

          <List.ListAction
            name={localization.archive.documents.cancel.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({type: 'error', content: localization.archive.documents.cancel.emptyMessage});
              } else {
                var allCanBeCancelled = _.all(selected, function(doc) {
                  return doc.field("status") == "pending" &&
                    (Utils.viewerIsAuthor(doc) || (doc.field("viewer").role == "company_admin"  && self.props.forCompanyAdmin)
                  );
                });
                if (!allCanBeCancelled) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.cancel.notAvailableMessage});
                } else {
                  self.openCancelModal(selected);
                }
              }
            }}
          />

          <List.ListAction
            name={localization.archive.documents.remove.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({type: 'error', content: localization.archive.documents.cancel.emptyMessage});
              } else {
                var canDeleteAll = _.all(selected, function (doc) {
                  return (doc.field("status") !== "pending" ||
                    (Utils.viewerIsAuthor(doc) || (doc.field("viewer").role == "company_admin"  && self.props.forCompanyAdmin)) ||
                    (Utils.currentViewerParty(doc) && Utils.signatoryCanSignNow(doc,Utils.currentViewerParty(doc)))
                  );
                });
                if (!canDeleteAll) {
                  new FlashMessage({type: 'error', content: localization.archive.documents.remove.invalidMessage});
                } else {
                  self.openRemoveModal(selected);
                }
              }
            }}
          />

          <List.ListSelectActions
            name={localization.more}
            width={270}
            actions={[
              {
                name: localization.archive.documents.csv.action,
                onSelect : function(listmodel) {
                  mixpanel.track('Download CSV');
                  var url = "/d/csv?";
                  var params =  listmodel.urlParams();
                  _.each(params,function(a,b){url+=(b+"="+a+"&");});
                  window.open(url);
                  return true;
                }
              },
              {
                name: localization.archive.documents.zip.action,
                onSelect : function(listmodel) {
                  mixpanel.track('Download PDFs');
                  var selected = listmodel.list().getSelected();
                  if (selected.length == 0 ) {
                    new FlashMessage({type: 'error', content : localization.archive.documents.zip.emptyMessage});
                    return true;
                  }
                  else if (selected.length == 1) {
                    var url =  "/api/frontend/downloadmainfile/" + selected[0].field("id") + "/" + encodeURIComponent(selected[0].field("title")) + ".pdf";
                    window.open(url);
                    return true;
                  } else {
                    var url =  "/d/zip?";
                    url += "documentids=[" + _.map(selected,function(s){return s.field("id");}) + "]";
                    window.open(url);
                    return true;
                  }
                }
              }
            ]}
         />

          {DocumentFilters({list:self})}
          {self.props.idledoctimeout == null ? "" :
            <List.ListSubHeader>
              {idleRemark}
            </List.ListSubHeader>}
          {DocumentColumns({list:self})}
           <List.ListFooter>
             <div className='table-statuses'>
               <div className='icon status float-left draft first'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.draft}</div>
               <div className='icon status float-left problem'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.cancelled}</div>
               <div className='icon status float-left sent'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.sent}</div>
               <div className='icon status float-left delivered'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.delivered}</div>
               <div className='icon status float-left read'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.read}</div>
               <div className='icon status float-left opened'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.opened}</div>
               <div className='icon status float-left signed'/>
               <div className='float-left'>{localization.archive.documents.statusDescription.signed}</div>
             </div>
          </List.ListFooter>
          <List.Pagination/>
        </List.List>
      );
    }
});
