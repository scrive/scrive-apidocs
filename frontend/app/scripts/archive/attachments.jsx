/** @jsx React.DOM */

define(['React','lists/list', 'moment', 'legacy_code','common/uploadbutton'], function(React, List,moment,_legacy,UploadButton) {


return React.createClass({
    mixins : [List.ReloadableContainer],
    attachmentDownloadLink : function(d) {
      return "/a/download/"+ d.field("id") + "/" + d.field("title") +".pdf";
    },
    openShareModal: function(selected) {
      var self = this;
      new Confirmation({
        acceptText: localization.ok,
        rejectText: localization.cancel,
        title: localization.archive.attachments.share.head,
        content: jQuery("<p/>").text(localization.archive.attachments.share.body),
        onAccept : function() {
           new Submit({
             url: "/a/share",
             method: "POST",
             attachmentids: "["+ _.map(selected, function(doc){return doc.field("id");}) + "]",
             ajaxsuccess : function() {
               new FlashMessage({type: 'success', content : localization.archive.attachments.share.successMessage});
               self.reload();
             }
          }).sendAjax();
          return true;
        }
      });
    },
    openRemoveModal : function(selected) {
      var self = this;
      var confirmationText = $('<span />').html(localization.archive.attachments.remove.body);
      var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
      if (selected.length == 1) {
        listElement.html($('<strong />').text(selected[0].field("title")));
      } else {
        listElement.text(selected.length + (" " + localization.attachments).toLowerCase());
      }
      new Confirmation({
        acceptText: localization.ok,
        rejectText: localization.cancel,
        title: localization.archive.attachments.remove.action,
        content: confirmationText,
        onAccept : function() {
          new Submit({
            url: "/a/delete",
            method: "POST",
            attachmentids: "["+ _.map(selected, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type: 'success', content : localization.archive.attachments.remove.successMessage});
              self.reload();
            }
          }).sendAjax();
          return true;
        }
     });
    },
    render: function() {
      var self = this;
      return (
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
                width={118}
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
      );
    }
});



});
