/* Definition of attachments archive */

(function(window){

window.AttachmentsListDefinition = function(archive) {
 return {
    name : "Attachments Table",
    schema: new Schema({
    url: "/a",
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
                            var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.attachments.share.head,
                                content: jQuery("<p/>").text(localization.archive.attachments.share.body),
                                onAccept : function() {
                                    new Submit({
                                                url: "/a/share",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.attachments.share.successMessage});
                                                    archive.attachments().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                            return true;
                          }
               },
               {name :  localization.archive.attachments.remove.action,
                onSelect: function(docs){
                             var confirmtext = jQuery("<p/>").append(localization.archive.attachments.remove.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.attachments).toLowerCase());
                             }
                             confirmtext.append("?");
                             var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.attachments.remove.action,
                                content: confirmtext,
                                onAccept : function() {
                                    var confirmationPopup = new Submit({
                                                url: "/a/delete",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.attachments.remove.successMessage});
                                                    archive.attachments().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax(); 
                                }
                              });
                              return true;
                          }
                }
              ]

    }),
    headerExtras: UploadButton.init({
                    size: "tiny",
                    width : "110",
                    text: localization.archive.attachments.createnew.action,
                    name : "doc",
                    onAppend : function(input) {
                       setTimeout(function() {
                        new Submit({
                          method : "POST",
                          url : "/a",
                          ajaxsuccess : function() {archive.attachments().recall();}
                        }).addInputs($(input)).sendAjax(); },100);
                    }
                  }).input().addClass("float-left")
 };
 
};
})(window);
