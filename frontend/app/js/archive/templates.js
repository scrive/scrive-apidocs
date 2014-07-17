/* Definition of templates archive */

define(['Backbone', 'legacy_code'], function() {

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

        new Cell({name: localization.archive.templates.columns.time, width:"105px", field:"time", special: "rendered",
                  rendering: function(time) {
                         return $("<span/>").text(new Date(Date.parse(time)).toYMDString()).attr("title",new Date(Date.parse(time)).fullTime());
                  }}),
        new Cell({name: localization.archive.templates.columns.verificationMethod, width:"150px", field:"id",  special: "rendered",
                  rendering: function(value, idx, model) {

                         var dms = model.field("deliveryMethods") || [];
                         dms = _.map(dms,function(dm) {
                           if (dm == "email")
                             return capitaliseFirstLetter(localization.delivery.email);
                           else if (dm == "pad")
                             return capitaliseFirstLetter(localization.delivery.pad);
                           else if (dm == "mobile")
                             return capitaliseFirstLetter(localization.delivery.mobile);
                           else if (dm == "email_mobile")
                             return capitaliseFirstLetter(localization.delivery.email_mobile);
                           else if (dm == "api")
                             return capitaliseFirstLetter(localization.delivery.api);
                           return "";
                         });
                         dms = _.uniq(dms);
                         dms.sort();

                         var text = dms[0] || "";
                         for(var i =1 ; i< dms.length; i++)
                            text += ", " + dms[i];

                         return  $("<div/>").text(text);
                  }}),
        new Cell({name: localization.archive.templates.columns.template, width:"535px", field:"title",  special: "link"}),
        new Cell({name: localization.archive.templates.columns.shared, width:"50px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         return $("<div/>").addClass((shared) ? "sharedIcon" : "notSharedIcon");
                  }})
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
                            var confirmationPopup = new Confirmation({
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
                                                    confirmationPopup.clear();
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
                        var confirmationText = $('<span />').html(localization.archive.templates.remove.body);
                        var listElement = confirmationText.find('.put-one-or-more-things-to-be-deleted-here');
                        if (docs.length == 1) {
                          listElement.html($('<strong />').text(docs[0].field("title")));
                        } else {
                          listElement.text(docs.length + (" " + localization.templates).toLowerCase());
                        }
                             var confirmationPopup = new Confirmation({
                                acceptText: localization.archive.templates.remove.action,
                                rejectText: localization.cancel,
                                title: localization.archive.templates.remove.action,
                                icon: '/img/modal-icons/delete.png',
                                content: confirmationText,
                                oneClick: true,
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/delete",
                                                method: "POST",
                                                documentids: "[" + _.map(docs, function(doc){return doc.field("id");}) + "]",
                                                ajaxsuccess : function() {
                                                    new FlashMessage({color : "green", content : localization.archive.templates.remove.successMessage});
                                                    archive.templates().recall();
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
