/* Definition of rubish bin archive */

(function(window){

window.BinListDefinition = function(archive) { return {
    name : "Trash table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Rubbish" },
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.bin.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.bin.columns.time, width:"140px", field:"time"}),
        new Cell({name: localization.archive.bin.columns.type, width:"120px", field:"type"}),
        new Cell({name: localization.archive.bin.columns.title, width:"400px", field:"title",  special: "link"})
        ],
    actions : [
      new ListAction({
                name : localization.archive.bin.restore.action,
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
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.bin.restore.successMessage});
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
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.bin.remove.successMessage});
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
