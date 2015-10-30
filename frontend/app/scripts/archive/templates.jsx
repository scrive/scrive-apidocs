/** @jsx React.DOM */

define(['React', 'archive/utils', 'lists/list', 'moment', 'legacy_code'], function(React, Utils, List, moment) {


return React.createClass({
    mixins : [List.ReloadableContainer],
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
            new FlashMessage({content: localization.couldNotUpload, type: 'error'});
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
              {"filter_by" : "template", "is_template" : true},
              {"filter_by" : "is_author"},
              {"filter_by" : "trash", "is_trashed" : false}
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

});
