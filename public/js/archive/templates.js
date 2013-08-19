/* Definition of templates archive */


(function(window){


window.TemplatesListDefinition = function(archive) { return {
    name : "Templatesarchive table",
    loadOnInit : false,
    schema: new Schema({
    url: "/api/frontend/list",
    extraParams : { documentType : "MyTemplate" },
    sorting: new Sorting({ fields: ["title", "time", "process"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.templates.search}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.templates.columns.shared, width:"60px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         return $("<div/>").addClass((shared) ? "sharedIcon" : "notSharedIcon");
                  }}),

        new Cell({name: localization.archive.templates.columns.time, width:"150px", field:"time", special: "rendered",
                  rendering: function(time) {
                         return $("<div/>").text(new Date(Date.parse(time)).toYMDString());
                  }}),
        new Cell({name: localization.archive.templates.columns.verificationMethod, width:"100px", field:"id",  special: "rendered",
                  rendering: function(value, idx, model) {
                         var res= $("<div/>");
                         if (model.field("authentication") == "eleg")
                             res.text(localization.eleg);
                         else if (model.field("delivery") == "pad")
                             res.text(localization.pad.delivery);
                         else
                             res.text(localization.email);
                         return res;
                  }}),
        new Cell({name: localization.archive.templates.columns.template, width:"400px", field:"title",  special: "link"})
        ],
    actions : [
       new ListAction({
                name : localization.archive.templates.createnew,
                avaible : function() {return true;},
                acceptEmpty : true,
                button: new Button({
                            color : "black",
                            width : "144",
                            text: localization.archive.templates.createnew,
                            onClick : function() {
                              new Submit({
                                method : "POST",
                                url : "/api/frontend/createfromfile",
                                ajax: true,
                                template: "YES",
                                expectedType : "text",
                                onSend: function() {
                                            LoadingDialog.open();
                                        },
                                ajaxsuccess: function(d) {
                                    try {
                                        window.location.href = "/d/"+JSON.parse(d).id;
                                    } catch(e) {
                                        new FlashMessage({content: localization.couldNotUpload, color: "red"});
                                        LoadingDialog.close();
                                    }
                                }
                            }).send();
                    }
            })
        }),
        new ListAction({
                name : localization.archive.templates.share.action,
                emptyMessage :  localization.archive.templates.share.emptyMessage,
                avaible : function() {return true;},
                onSelect: function(docs){
                            var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.templates.share.head,
                                icon: '/img/modal-icons/multisend.png',
                                content: jQuery("<p/>").text(localization.archive.templates.share.body),
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/share",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.templates.share.successMessage});
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
                emptyMessage :  localization.archive.templates.remove.emptyMessage,
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
                                icon: '/img/modal-icons/sign.png',
                                content: confirmtext,
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/delete",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.templates.remove.successMessage});
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
    })
};};


})(window);
