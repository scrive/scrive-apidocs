/** @jsx React.DOM */

define(['React','archive/utils','archive/statustooltipmixin','lists/list','archive/document_columns','archive/document_filters','legacy_code'], function(React, Utils, StatusTooltipMixin, List, DocumentColumns,DocumentFilters) {


return React.createClass({
    mixins: [StatusTooltipMixin,List.ReloadableContainer],
    openRestoreModal : function(selected) {
      var self = this;
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.trash.restore.head,
        rejectText: localization.cancel,
        title: localization.archive.trash.restore.head,
        content: jQuery("<p class='center'/>").text(localization.archive.trash.restore.body),
        oneClick: true,
        onAccept : function() {
          new Submit({
            url: "/d/restore",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
              ajaxsuccess : function() {
                new FlashMessage({
                  color : "green", content : localization.archive.trash.restore.successMessage});
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
      var confirmationText = $('<span />').html(localization.archive.trash.remove.body);
      confirmationText.append(" ");
      confirmationText.append($('<span />').html(localization.archive.trash.remove.cannotUndo));
      var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
      if (selected.length == 1) {
        listElement.html($('<strong />').text(selected[0].field("title")));
      } else {
        listElement.text(selected.length + (" " + localization.documents).toLowerCase());
      }
      var confirmationPopup = new Confirmation({
        acceptText: localization.archive.trash.remove.action,
        rejectText: localization.cancel,
        title: localization.archive.trash.remove.action,
        content: confirmationText,
        onAccept : function() {
          mixpanel.track('Really delete document');
          new Submit({
            url: "/d/reallydelete",
            method: "POST",
            documentids: "[" + _.map(selected, function(doc){return doc.field("id");}) + "]",
            ajaxsuccess : function() {
              new FlashMessage({type: "success", content : localization.archive.trash.remove.successMessage});
              self.reload();
              confirmationPopup.clear();
            }
          }).sendAjax();
        }
      });
    },
    render: function() {
      var self = this;
      return (
        <List.List
          maxPageSize={Utils.maxPageSize}
          totalCountFunction={Utils.totalCountFunction}
          url={Utils.listCallUrl}
          paramsFunction={Utils.paramsFunctionWithFilter([
              {"filter_by" : "trash", "is_trashed" : true},
              {"filter_by" : "template", "is_template" : false}
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

          {DocumentFilters({list:self})}
          <List.ListSubHeader>
            {localization.archive.trash.subheadline}
          </List.ListSubHeader>
          {DocumentColumns({list:self})}
          <List.Pagination/>
        </List.List>
      );
    }
});



});
