/* Definition of rubish bin archive */

(function(window){

window.BinListDefinition = {
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
    options : [{name : localization.archive.bin.restore.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                restore: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.restore.head,
                                content: jQuery("<p/>").text(localization.archive.bin.restore.body)
                              })
                          }
               },
               {name : localization.archive.bin.delete.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                reallydelete: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                              Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.bin.delete.head,
                                content: jQuery("<p/>").text(localization.archive.bin.delete.body)
                              })
                          }
                }
              ]

    })

};

})(window);
