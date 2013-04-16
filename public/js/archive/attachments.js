/* Definition of attachments archive */

(function(window){

window.AttachmentsListDefinition = function(archive) {
 return {
    name : "Attachments Table",
    loadOnInit : false,
    schema: new Schema({
    url: "/a",
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.attachments.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.attachments.columns.shared, width:"60px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         return $("<div/>").addClass((shared) ? "sharedIcon" : "notSharedIcon");
                  }}),
        new Cell({name: localization.archive.attachments.columns.attachment, width:"600px", field:"title",  special: "link"}),
        new Cell({name: localization.archive.attachments.columns.time, width:"140px", field:"time", special: "rendered",
                  rendering: function(time) {
                         return $("<div/>").text(new Date(Date.parse(time)).toTimeAbrev());
                  }})
        ],
    actions : [
        new ListAction({
                name :  localization.archive.attachments.createnew.action,
                avaible : function(){return true;},
                acceptEmpty : true,
                button: UploadButton.init({
                            size: "tiny",
                            width : 144,
                            color : "black",
                            text: localization.archive.attachments.createnew.action,
                            name : "doc",
                            onAppend : function(input) {
                            setTimeout(function() {
                                new Submit({
                                method : "POST",
                                url : "/a",
                                ajaxsuccess : function() {archive.attachments().recall();},
                                ajaxerror : function() {
                                  new FlashMessage({color: "red", content: localization.couldNotUpload});
                                }
                                }).addInputs($(input)).sendAjax(); },100);

                            }
                        })
               }),
        new ListAction({
                name :  localization.archive.attachments.share.action,
                emptyMessage :  localization.archive.attachments.share.emptyMessage,
                avaible : function(){return true;},
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
                                                    new FlashMessage({color : "green", content : localization.archive.attachments.share.successMessage});
                                                    archive.attachments().recall();
                                                }
                                          }).sendAjax();
                                    return true;
                                }
                              });
                            return true;
                          }
               }),
        new ListAction({
                name :  localization.archive.attachments.remove.action,
                emptyMessage :  localization.archive.attachments.remove.emptyMessage,
                avaible : function(){return true;},
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
                                                    new FlashMessage({color : "green", content : localization.archive.attachments.remove.successMessage});
                                                    archive.attachments().recall();
                                                }
                                          }).sendAjax();
                                    return true;
                                }
                              });
                              return true;
                          }
                })
              ]

    })
 };

};
})(window);
