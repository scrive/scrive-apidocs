/* Definition of attachments archive */

(function(window){

window.AttachmentsListDefinition = {
    name : "Attachments Table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Attachment" },
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.attachments.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.attachments.columns.time, width:"140px", field:"time"}),
        new Cell({name: localization.archive.attachments.columns.attachment, width:"400px", field:"title",  special: "link"}),
        new Cell({name: "", width:"100px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         var res = jQuery("<p/>")
                         if (shared == "True")
                          return res.text(localization.archive.attachments.shared);
                         return res;
                  }})
        ],
    options : [{name :  localization.archive.attachments.share.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                share: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.attachments.share.head,
                                content: jQuery("<p/>").text(localization.archive.attachments.share.body)
                              })
                          }
               },
               {name :  localization.archive.attachments.delete.action,
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remove: "true",
                                                archive: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var confirmtext = jQuery("<p/>").append(localization.archive.attachments.delete.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.attachments).toLowerCase());
                             }
                             confirmtext.append("?");
                             Confirmation.popup({
                                submit: submit,
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.attachments.delete.action,
                                content: confirmtext
                              });
                          }
                }
              ]

    }),
    headerExtras: UploadButton.init({
                    size: "tiny",
                    width : "110",
                    text: localization.archive.attachments.createnew,
                    name : "doc",
                    submitOnUpload : true,
                    submit: new Submit({
                          method : "POST",
                          url : "$currentlink$"
                        })
                    }).input()
 };

var BinListDefinition = {
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
