/* Definition of rubish bin archive */

(function(window){


window.BinListDefinition = function(archive) { return {
    name : "Trash table",
    loadOnInit : false,
    schema: new Schema({
    url: "/api/frontend/list",
    extraParams : { documentType : "Rubbish" },
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
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
                            var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.restore.head,
                                content: jQuery("<p/>").text(localization.archive.bin.restore.body),
                                onAccept : function() {
                                  new Submit({
                                                url: "/d/restore",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.bin.restore.successMessage});
                                                    archive.bin().recall();
                                                    confirmationPopup.view.clear();
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
                avaible : function() {return true;},
                onSelect: function(docs){
                              var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.remove.head,
                                content: jQuery("<p/>").text(localization.archive.bin.remove.body),
                                onAccept : function() {
                                  new Submit({
                                                url: "/d/reallydelete",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.bin.remove.successMessage});
                                                    archive.bin().recall();
                                                    confirmationPopup.view.clear();
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

})(window);
