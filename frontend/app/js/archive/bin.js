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
               })
              ]

    })

};};

});
