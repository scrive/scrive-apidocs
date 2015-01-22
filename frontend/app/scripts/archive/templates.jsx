/** @jsx React.DOM */

define(['React', 'lists/list', 'moment', 'legacy_code'], function(React, List, moment) {


return React.createClass({
    mixins : [List.ReloadableContainer],
    verificationMethodText : function(d) {
      var dms = d.field("fields").deliveryMethods || [];
      dms = _.map(dms,function(dm) {
                        if (dm == "email") {
                         return capitaliseFirstLetter(localization.delivery.email);
                        } else if (dm == "pad") {
                         return capitaliseFirstLetter(localization.delivery.pad);
                        } else if (dm == "mobile") {
                         return capitaliseFirstLetter(localization.delivery.mobile);
                        } else if (dm == "email_mobile") {
                         return capitaliseFirstLetter(localization.delivery.email_mobile);
                        } else if (dm == "api") {
                         return capitaliseFirstLetter(localization.delivery.api);
                        } else {
                         return "";
                        }
      });
      dms = _.uniq(dms);
      dms.sort();

      var text = dms[0] || "";
      for(var i =1 ; i< dms.length; i++)
        text += ", " + dms[i];
      return text;
    },
    createNewTemplate : function() {
      new Submit({
        method : "POST",
        url : "/api/frontend/createfromfile",
        ajax: true,
        template: "YES",
        expectedType : "text",
        onSend: function() {
          LoadingDialog.open();
        },
        ajaxsuccess: function(d) {
          try {
            window.location.href = "/d/"+JSON.parse(d).id;
          } catch(e) {
            new FlashMessage({content: localization.couldNotUpload, color: "red"});
            LoadingDialog.close();
          }
        }
      }).send();
    },
    openShareModal: function(selected) {
      var self = this;
      var confirmationPopup = new Confirmation({
          acceptText: localization.ok,
          rejectText: localization.cancel,
          title: localization.archive.templates.share.head,
          icon: '/img/modal-icons/multisend.png',
          content: jQuery("<p/>").text(localization.archive.templates.share.body),
          onAccept : function() {
            new Submit({
              url: "/d/share",
              method: "POST",
              documentids: "[" + _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
              ajaxsuccess : function() {
                new FlashMessage({color : "green", content : localization.archive.templates.share.successMessage});
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
        listElement.html($('<strong />').text(selected[0].field("fields").title));
      } else {
        listElement.text(selected.length + (" " + localization.templates).toLowerCase());
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.templates.remove.action,
        rejectText: localization.cancel,
        title: localization.archive.templates.remove.action,
        icon: '/img/modal-icons/delete.png',
        content: confirmationText,
        oneClick: true,
        onAccept : function() {
          new Submit({
            url: "/d/delete",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("fields").id;}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({color : "green", content : localization.archive.templates.remove.successMessage});
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
          url='/api/frontend/list?documentType=MyTemplate'
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
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
                new FlashMessage({color: "red", content: localization.archive.templates.share.emptyMessage});
                return false;
              }
              self.openShareModal(selected);
            }}
          />

          <List.ListAction
            name={localization.archive.templates.remove.action}
            onSelect={function(selected,model) {
              if (selected.length ==0 ) {
                new FlashMessage({color: "red", content: localization.archive.templates.remove.emptyMessage});
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
            sorting="time"
            rendering={function(d) {
              var time = moment(d.field("fields").time).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.verificationMethod}
            width="150px"
            rendering={function(d) {
              return (<div>{self.verificationMethodText(d)}</div>);
            }}
          />
         <List.Column
            name={localization.archive.templates.columns.template}
            width="535px"
            sorting="title"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").title}</a>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.shared}
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
