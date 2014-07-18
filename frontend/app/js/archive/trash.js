/* Definition of document trash */

define(['Backbone', 'legacy_code'], function() {

window.TrashListDefinition = function(archive) { return {
    name : "Trash table",
    loadOnInit : false,
    schema: new Schema({
    url: "/api/frontend/list",
    extraParams : { documentType : "Rubbish" },
    sorting: new Sorting({ fields: ["title", "time"]}),
    paging: new Paging({}),
    subheadline: localization.archive.trash.subheadline,
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.trash.search}),
    selectfiltering : new DocumentSelectsDefinition(archive),
    cells : new DocumentCellsDefinition(archive,false),
    actions : [
      new ListAction({
                name : localization.archive.trash.restore.action,
                emptyMessage :  localization.archive.trash.restore.emptyMessage,
                avaible : function() {return true;},
                onSelect: function(docs){
                            var confirmationPopup = new Confirmation({
                                acceptText: localization.archive.trash.restore.head,
                                rejectText: localization.cancel,
                                title: localization.archive.trash.restore.head,
                                icon: '/img/modal-icons/restore.png',
                                content: jQuery("<p class='center'/>").text(localization.archive.trash.restore.body),
                                oneClick: true,
                                onAccept : function() {
                                  new Submit({
                                                url: "/d/restore",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.trash.restore.successMessage});
                                                    archive.trash().recall();
                                                    confirmationPopup.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                            return true;
                          }
               }),
        new ListAction({
            name : localization.archive.trash.remove.action,
            emptyMessage :  localization.archive.trash.remove.emptyMessage,
            size: 'normal',
            avaible : function(doc){ return true;},
            onSelect : function(docs) {
                        var confirmationText = $('<span />').html(localization.archive.trash.remove.body);
                        confirmationText.append(" ");
                        confirmationText.append($('<span />').html(localization.archive.trash.remove.cannotUndo));
                        var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
                        if (docs.length == 1) {
                          listElement.html($('<strong />').text(docs[0].field("title")));
                        } else {
                          listElement.text(docs.length + (" " + localization.documents).toLowerCase());
                        }
                             var confirmationPopup = new Confirmation({
                                acceptText: localization.archive.trash.remove.action,
                                rejectText: localization.cancel,
                                title: localization.archive.trash.remove.action,
                                icon: '/img/modal-icons/delete.png',
                                content: confirmationText,
                                onAccept : function() {
                                    mixpanel.track('Really delete document');
                                    new Submit({
                                                url: "/d/reallydelete",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.trash.remove.successMessage});
                                                    archive.trash().recall();
                                                    confirmationPopup.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                             return true;
            }
        })
              ]

    })

};};

});
