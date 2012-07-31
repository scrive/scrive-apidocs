/* Definition of documents archive */


(function(window){

window.DocumentsListDefinition = function(archive) { return {
    name : "Documents Table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Document" },
    sorting: new Sorting({ fields: ["title", "status", "time", "party", "author"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.documents.search}),
    selectfiltering : [
            new SelectFiltering({
                             name: "status",
                             textWidth : "100px",
                             options: [ {name: localization.filterByStatus.showAnyStatus, value: ""},
                                        {name: localization.filterByStatus.showDraft,     value: "[draft]"},
                                        {name: localization.filterByStatus.showCancelled, value: "[cancelled]"},
                                        {name: localization.filterByStatus.showSent,      value: "[sent,delivered,read,opened]"},
                                        {name: localization.filterByStatus.showSigned,    value: "[signed]"}
                                      ]}), 
            new SelectFiltering({
                             name: "process",
                             textWidth : "100px",
                             options: [ {name: localization.filterByProcess.showAllProcesses,  value: ""},
                                        {name: localization.filterByProcess.showContractsOnly, value: "contract"},
                                        {name: localization.filterByProcess.showOffersOnly,    value: "offer"},
                                        {name: localization.filterByProcess.showOrdersOnly,    value: "order"}
                                      ]}),
            new IntervalDoubleSelectFiltering({
                             name: "time",
                             textWidth : "110px",
                             selectedBottomPrefix : localization.filterByTime.filterForm,
                             selectedTopPrefix :    localization.filterByTime.filterTo ,
                             options: function() {
                                        var year = archive.year();
                                        var month = archive.month();
                                        var options = [{name : localization.filterByTime.filterForm , value : "<" }];
                                        var time = new Date();
                                        while (year < time.getFullYear() || (year == time.getFullYear() && month <= time.getMonth() + 1)) {
                                            var name = capitaliseFirstLetter(localization.months[month-1].slice(0,3) + " " + year);
                                            options.push({name : name , value : "("+month + "," + year + ")" })
                                            month++;
                                            if (month == 13) {month = 1; year++;}
                                        };
                                        options.push({name : localization.filterByTime.filterTo , value : ">" });
                                        return options} ()
                             })
            ], 
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: localization.archive.documents.columns.status, width:"52px", field:"status",
                 rendering: function(status,idx,listobject) {
                    var icon = jQuery("<div class='icon status "+status+"'></div>")
                    var tip = jQuery("<div id='tooltip-"+status+"'> <div class='icon status "+status+"'></div><p>"+
                                      localization.statusToolTip[status]+"</p></div>");
                    ToolTip.set({
                        on: icon,
                        tip: tip
                    })
                    if ((listobject.field("anyinvitationundelivered") == "True" &&  idx == undefined)
                       || (idx != undefined && listobject.subfield(idx,"invitationundelivered") == "True"))
                       icon = jQuery.merge( icon, jQuery("<span style='color:#000000;position:relative;top:-2px'>!</span>"));

                    return icon;
                 }
        }),
        new Cell({name: localization.archive.documents.columns.time, width:"116px", field:"time"}),
        new Cell({name: localization.archive.documents.columns.sender, width:"110px", field:"author",  special: "link"}),
        new Cell({width:"5px" }),
        new Cell({name: localization.archive.documents.columns.party, width:"190px", field:"party", special: "expandable", subfield : "name"}),
        new Cell({width:"5px" }),
        new Cell({name: localization.archive.documents.columns.title, width:"240px", field:"title",  special: "link"}),
        new Cell({width:"5px" }),
        new Cell({name: localization.archive.documents.columns.type, width:"40px", field:"process",
                  rendering: function(value, _idx, _model) {
                      var txt = "";
                      if( localization.process[value] !== undefined ) {
                          txt = localization.process[value].shortName;
                      }
                      return jQuery("<span />").text(txt);
                    }
                 })
        ],
    actions : [
           new ListAction({
                name : localization.archive.documents.createnew,
                color : "green",
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
                                    url : "/d",
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
                            title: localization.archive.documents.createnewtype,
                            content: table
                        });
                        popup.hideAccept();
                        return false;
                    }
            }),
        new ListAction({
            name :  localization.archive.documents.sendreminder.action,
            color : "green",
            avaible : function(doc){
              return doc.field("status") == "sent"      ||
                     doc.field("status") == "delivered" ||
                     doc.field("status") == "read"      ||
                     doc.field("status") == "opened"
            },
            onSelect : function(docs) {
                 var content = jQuery("<p/>");
                             if (docs.length == 1) {
                               content.append(localization.archive.documents.sendreminder.bodysingle + " ");
                               content.append(jQuery("<strong/>").text(docs[0].field("title")));
                               content.append("?");
                             } else {
                               content.text(localization.archive.documents.sendreminder.bodymulti + " "+ docs.length + (" " + localization.documents +"?").toLowerCase());
                             }
                             var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.documents.sendreminder.action,
                                content: content,
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/remind",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.documents.sendreminder.successMessage});
                                                    archive.documents().recall();
                                                    confirmationPopup.view.clear();
                                                },
                                                ajaxerror : function() {
                                                    FlashMessages.add({color : "red", content : localization.archive.documents.sendreminder.errorMessage});
                                                    archive.documents().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                             return true;
            }
        }),
        new ListAction({
            name :  localization.archive.documents.cancel.action,
            color : "red",
            avaible : function(doc){
              return doc.field("status") == "sent"      ||
                     doc.field("status") == "delivered" ||
                     doc.field("status") == "read"      ||
                     doc.field("status") == "opened"
            },
            onSelect : function(docs) {
                             var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.documents.cancel.action,
                                content: jQuery("<p/>").text(localization.archive.documents.cancel.body),
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/cancel",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.documents.cancel.successMessage});
                                                    archive.documents().recall();
                                                    confirmationPopup.view.clear();
                                                },
                                                ajaxerror : function() {
                                                    archive.documents().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                             return true;
            }
        }),
        new ListAction({
            name : localization.archive.documents.remove.action,
            color : "black",
            avaible : function(doc){ return true;},
            onSelect : function(docs) {
                         var confirmtext = jQuery("<p/>").append(localization.archive.documents.remove.body + " ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" " + localization.documents).toLowerCase());
                             }
                             confirmtext.append("?");
                             var confirmationPopup = Confirmation.popup({
                                acceptText: localization.ok,
                                rejectText: localization.cancel,
                                title: localization.archive.documents.remove.action,
                                content: confirmtext,
                                onAccept : function() {
                                    new Submit({
                                                url: "/d/delete",
                                                method: "POST",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");}),
                                                ajaxsuccess : function() {
                                                    FlashMessages.add({color : "green", content : localization.archive.documents.remove.successMessage});
                                                    archive.documents().recall();
                                                    confirmationPopup.view.clear();
                                                }
                                          }).sendAjax();
                                }
                              });
                             return true;
            }
        })
    ],
    options : [
                {name : localization.archive.documents.csv.action,
                 acceptEmpty : true,
                 onSelect: function(){
                        var url =  archive.documents().model.schema.url() + "?"
                        var params =  archive.documents().model.schema.getSchemaUrlParams();
                        _.each(params,function(a,b){url+=(b+"="+a+"&")})
                        window.open(url + "format=csv");
                        return true;
                 }
                } ,
               {name : localization.archive.documents.zip.action,
                 acceptEmpty : true,
                 onSelect: function(){
                        var url =  archive.documents().model.schema.url() + "?"
                        var params =  archive.documents().model.schema.getSchemaUrlParams();
                        _.each(params,function(a,b){url+=(b+"="+a+"&")})
                        window.open(url + "format=zip");
                        return true;
                 }
                } 
              ]
    }),
    bottomExtras : function() {
                        var box = $("<div class='table-statuses'/>");
                        var description = function(cssClass,text) {
                            var icon = $("<div class='icon status float-left'></div>").addClass(cssClass);
                            var text = $("<div class='float-left'/>").text(text);
                            return $.merge(icon,text);
                        };
                        box.append(description("draft",localization.archive.documents.statusDescription.draft));
                        box.append(description("cancelled",localization.archive.documents.statusDescription.cancelled));
                        box.append(description("sent",localization.archive.documents.statusDescription.sent));
                        box.append(description("delivered",localization.archive.documents.statusDescription.delivered));
                        box.append(description("read",localization.archive.documents.statusDescription.read));
                        box.append(description("opened",localization.archive.documents.statusDescription.opened));
                        box.append(description("signed",localization.archive.documents.statusDescription.signed));
                        return box;
                    }()
};};


})(window);
