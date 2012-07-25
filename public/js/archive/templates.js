/* Definition of templates archive */


(function(window){


window.TemplatesListDefinition = function(archive) { return {
    name : "Templatesarchive table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Template" },
    sorting: new Sorting({ fields: ["title", "time", "process"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.templates.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.templates.columns.time, width:"140px", field:"time"}),
        new Cell({name: localization.archive.templates.columns.type, width:"120px", field:"process"}),
        new Cell({name: localization.archive.templates.columns.template, width:"400px", field:"title",  special: "link"}),
        new Cell({name: "", width:"100px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         var res = jQuery("<p/>")
                         if (shared == "True")
                          return res.text(localization.archive.templates.shared);
                         return res;
                  }})
        ],
    actions : [
       new ListAction({
                name : localization.archive.templates.createnew,
                avaible : function() {return true;},
                acceptEmpty : true,
                onSelect: function() {
                        var popup;
                        function doctypebutton(txt,type) {
                            return jQuery('<td/>').append(jQuery('<div class="documentTypeBox"/>').append(UploadButton.init({
                                size: "tiny",
                                width : "130",
                                text: txt, //
                                button: jQuery('<a href="#" class="documenticon withUpload"/>').append(jQuery('<div class="documenticonText"/>').append(jQuery("<span class='text'/>").text(txt))),
                                name : "doc",
                                submitOnUpload : true,
                                submit: new Submit({
                                    method : "POST",
                                    doctype: type,
                                    url : "/t",
                                    onSend: function() {LoadingDialog.open();},
                                })
                            }).input()));
                        }
                        var t = jQuery('<tr/>') ;
                        t.append(doctypebutton(localization.process.contract.name, "Contract"));
                        t.append(doctypebutton(localization.process.offer.name, "Offer"));
                        t.append(doctypebutton(localization.process.order.name, "Order"));
                        var table = jQuery('<table style="width: 100%"/>').append(jQuery('<tbody/>').append(t));
                        popup = Confirmation.popup({
                            onAccept: function() { },
                            title: localization.archive.templates.createnewtype,
                            content: table
                        });
                        popup.hideAccept();
                        return false;
                    }
            }),
        new ListAction({
                name : localization.archive.templates.share.action,
                avaible : function() {return true;},
                onSelect: function(docs){
                            var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.templates.share.head,
                                content: jQuery("<p/>").text(localization.archive.templates.share.body),
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/share",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.templates.share.successMessage});
                                                    archive.templates().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                            return true;
                          }
    
            }),
        new ListAction({
                name : localization.archive.templates.remove.action,
                avaible : function() {return true;},
                onSelect: function(docs){
                             var confirmtext = jQuery("<p/>").append(localization.archive.templates.remove.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.templates).toLowerCase());
                             }
                             confirmtext.append("?");
                             var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.templates.remove.action,
                                content: confirmtext,
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/delete",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.templates.remove.successMessage});
                                                    archive.templates().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                            return true;
                          }
            })           
        ]
    }),    
};};


})(window);
