/* Definition of rubish bin archive */

define(['Backbone', 'legacy_code'], function() {

window.BinListDefinition = function(archive) { return {
    name : "Trash table",
    loadOnInit : false,
    schema: new Schema({
    url: "/api/frontend/list",
    extraParams : { documentType : "Rubbish" },
    sorting: new Sorting({ fields: ["title", "time"]}),
    paging: new Paging({}),
    subheadline: localization.archive.bin.subheadline,
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.bin.search}),
    selectfiltering : new DocumentSelectsDefinition(archive),
    cells : new DocumentCellsDefinition(archive,false),
    actions : [
      new ListAction({
                name : localization.archive.bin.restore.action,
                emptyMessage :  localization.archive.bin.restore.emptyMessage,
                avaible : function() {return true;},
                onSelect: function(docs){
                            var confirmationPopup = new Confirmation({
                                acceptText: localization.archive.bin.restore.head,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.restore.head,
                                icon: '/img/modal-icons/restore.png',
                                content: jQuery("<p class='center'/>").text(localization.archive.bin.restore.body),
                                oneClick: true,
                                onAccept : function() {
                                  new Submit({
                                                url: "/d/restore",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.bin.restore.successMessage});
                                                    archive.bin().recall();
                                                    confirmationPopup.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                            return true;
                          }
               }),
        new ListAction({
            name : localization.archive.bin.remove.action,
            emptyMessage :  localization.archive.bin.remove.emptyMessage,
            size: 'normal',
            avaible : function(doc){ return true;},
            onSelect : function(docs) {
                        var confirmationText = $('<span />').html(localization.archive.bin.remove.body);
                        confirmationText.append(" ");
                        confirmationText.append($('<span />').html(localization.archive.bin.remove.cannotUndo));
                        var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
                        if (docs.length == 1) {
                          listElement.html($('<strong />').text(docs[0].field("title")));
                        } else {
                          listElement.text(docs.length + (" " + localization.documents).toLowerCase());
                        }
                             var confirmationPopup = new Confirmation({
                                acceptText: localization.archive.bin.remove.action,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.remove.action,
                                icon: '/img/modal-icons/delete.png',
                                content: confirmationText,
                                onAccept : function() {
                                    mixpanel.track('Really delete document');
                                    new Submit({
                                                url: "/d/reallydelete",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.bin.remove.successMessage});
                                                    archive.bin().recall();
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
