/** @jsx React.DOM */

define(['React', 'archive/statustooltipmixin','lists/list','archive/document_columns','archive/document_filters','legacy_code'], function(React, StatusTooltipMixin, List, DocumentColumns,DocumentFilters) {


return React.createClass({
    mixins: [StatusTooltipMixin,List.ReloadableContainer],
    openSendReminderModal : function(selected) {
      var self = this;
      var content = jQuery("<p/>");
      if (selected.length == 1) {
        var span = $('<span />').html(localization.archive.documents.sendreminder.bodysingle);
        span.find('.put-document-name-here').html(jQuery("<strong/>").text(selected[0].field("fields").title));
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
        icon: '/img/modal-icons/remind.png',
        content: content,
        onAccept : function() {
          mixpanel.track('Send reminder');
          new Submit({
            url: "/d/remind",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({color : "green", content : localization.archive.documents.sendreminder.successMessage});
              self.reload();
              confirmationPopup.clear();
            },
            ajaxerror : function() {
              new FlashMessage({color : "red", content : localization.archive.documents.sendreminder.errorMessage});
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
        icon: '/img/modal-icons/sign.png',
        content: jQuery("<p/>").text(localization.archive.documents.cancel.body),
        onAccept : function() {
          mixpanel.track('Cancel document');
          new Submit({
            url: "/d/cancel",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({color : "green", content : localization.archive.documents.cancel.successMessage});
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
        listElement.html($('<strong />').text(selected[0].field("fields").title));
      } else {
        listElement.text(selected.length + (" " + localization.documents).toLowerCase());
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.documents.remove.action,
        rejectText: localization.cancel,
        title: localization.archive.documents.remove.action,
        icon: '/img/modal-icons/delete.png',
        content: confirmationText,
        oneClick: true,
        onAccept : function() {
          mixpanel.track('Delete document');
          new Submit({
            url: "/d/delete",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({color : "green", content : localization.archive.documents.remove.successMessage});
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
          url='/api/frontend/list?documentType=Document'
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >
          <List.TextFiltering text={localization.archive.documents.search} />

          <List.ListAction
            name={localization.archive.documents.sendreminder.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({color: "red", content: localization.archive.documents.sendreminder.emptyMessage});
                return false;
              }
              var allSelectedArePending = _.all(selected, function(doc) {
                     return doc.field("fields").status == "sent"      ||
                            doc.field("fields").status == "delivered" ||
                            doc.field("fields").status == "read"      ||
                            doc.field("fields").status == "opened";
              });
              if (!allSelectedArePending) {
                new FlashMessage({color: "red", content: localization.archive.documents.sendreminder.notAvailableMessage});
                return false;
              }
              self.openSendReminderModal(selected);
            }}
          />

          <List.ListAction
            name={localization.archive.documents.cancel.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({color: "red", content: localization.archive.documents.cancel.emptyMessage});
                return false;
              }
              var allCanBeCancelled = _.all(selected, function(doc) {
                return _.contains(['sent', 'delivered', 'read', 'opened'], doc.field('fields').status) && (doc.field('isauthor') || ((doc.field('docauthorcompanysameasuser')  && self.props.forCompanyAdmin)));
              });
              if (!allCanBeCancelled) {
                new FlashMessage({color: "red", content: localization.archive.documents.cancel.notAvailableMessage});
                return false;
              }
              self.openCancelModal(selected);
            }}
          />

          <List.ListAction
            name={localization.archive.documents.remove.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({color: "red", content: localization.archive.documents.cancel.emptyMessage});
                return false;
              }
              self.openRemoveModal(selected);
            }}
          />

          <List.ListSelectActions
            name={localization.more}
            textWidth="243px"
            optionsWidth="270px"
            actions={[
              {
                name: localization.archive.documents.csv.action,
                onSelect : function(listmodel) {
                  mixpanel.track('Download CSV');
                  var url =  listmodel.url + "?";
                  var params =  listmodel.urlParams();
                  _.each(params,function(a,b){url+=(b+"="+a+"&")});
                    window.open(url + "format=csv");
                  return true;
                }
              },
              {
                name: localization.archive.documents.zip.action,
                onSelect : function(listmodel) {
                  mixpanel.track('Download PDFs');
                  var selected = listmodel.list().getSelected();
                  if (selected.length == 0 ) {
                    new FlashMessage({color : "red", content : localization.archive.documents.zip.emptyMessage});
                    return true;
                  }
                  else if (selected.length == 1) {
                    var url =  "/api/frontend/downloadmainfile/" + selected[0].field("fields").id + "/" + encodeURIComponent(selected[0].field("fields").title) + ".pdf";
                    window.open(url);
                    return true;
                  } else {
                    var url =  "/d/zip?";
                    url += "documentids=[" + _.map(selected,function(s){return s.field("fields").id}) + "]";
                    window.open(url);
                    return true;
                  }
                }
              }
            ]}
         />

          <DocumentFilters list={self}/>
          <DocumentColumns list={self}/>
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



});
