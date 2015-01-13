/** @jsx React.DOM */

define(['React','lists/list', 'moment', 'legacy_code'], function(React, List, moment) {


return React.createClass({
    mixins : [List.ReloadableContainer],
    openShareModal: function(selected) {
      var self = this;
      new Confirmation({
        acceptText: localization.ok,
        rejectText: localization.cancel,
        title: localization.archive.attachments.share.head,
        icon: '/img/modal-icons/multisend.png',
        content: jQuery("<p/>").text(localization.archive.attachments.share.body),
        onAccept : function() {
           new Submit({
             url: "/a/share",
             method: "POST",
             attachmentids: "["+ _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
             ajaxsuccess : function() {
               new FlashMessage({color : "green", content : localization.archive.attachments.share.successMessage});
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
        listElement.html($('<strong />').text(selected[0].field("fields").title));
      } else {
        listElement.text(selected.length + (" " + localization.attachments).toLowerCase());
      }
      new Confirmation({
        acceptText: localization.ok,
        rejectText: localization.cancel,
        title: localization.archive.attachments.remove.action,
        icon: '/img/modal-icons/delete.png',
        content: confirmationText,
        onAccept : function() {
          new Submit({
            url: "/a/delete",
            method: "POST",
            attachmentids: "["+ _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({color : "green", content : localization.archive.attachments.remove.successMessage});
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
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >
         <List.TextFiltering text={localization.archive.attachments.search} />

         <List.ListAction
            elFunction={function(model) {
              return new UploadButton({
                 text: localization.archive.attachments.createnew.action,
                 name : "doc",
                 color: "black",
                 width: 150,
                 onAppend : function(input) {
                   setTimeout(function() {
                                new Submit({
                                method : "POST",
                                url : "/a",
                                ajaxsuccess : function() {self.reload();},
                                ajaxerror : function() {
                                  new FlashMessage({color: "red", content: localization.couldNotUpload});
                                }
                                }).addInputs($(input)).sendAjax(); },100);

                }
              }).el()}}
          />

          <List.ListAction
            name={localization.archive.attachments.share.action}
            width={50}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({color: "red", content: localization.archive.attachments.share.emptyMessage});
                return false;
              }
              self.openShareModal(selected);
            }}
          />

          <List.ListAction
            name={localization.archive.attachments.remove.action}
            width={50}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({color: "red", content: localization.archive.attachments.remove.emptyMessage});
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
              var time = moment(d.field("fields").time).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />
          <List.Column
            name={localization.archive.attachments.columns.attachment}
            width="680px"
            sorting="title"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").title}</a>);
            }}
          />
          <List.Column
            name={localization.archive.attachments.columns.shared}
            width="75px"
            className="archive-table-shared-column-header"
            rendering={function(d) {
              return (<div className={"archive-table-shared-column " + ((d.field("fields").shared) ? "sharedIcon" : "")}/>);
            }}
          />

          <List.Pagination/>
        </List.List>
      );
    }
});



});
