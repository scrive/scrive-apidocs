/* Definition of documents archive */


(function(window){


window.DocumentCellsDefinition = function(archive) { return  [
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
        ]
};

window.DocumentSelectsDefinition = function(archive, draftsAvaible) { return  _.flatten([
            new SelectFiltering({
                             name: "status",
                             textWidth : "90px",
                             options: _.union(
                                        [{name: localization.filterByStatus.showAnyStatus, value: ""} ],
                                         (draftsAvaible ? [{name: localization.filterByStatus.showDraft,     value: "[draft]"}] : []),
                                        [{name: localization.filterByStatus.showCancelled, value: "[cancelled,rejected,timeouted,deliveryproblem,problem]"},
                                         {name: localization.filterByStatus.showSent,      value: "[sent,delivered,read,opened]"},
                                         {name: localization.filterByStatus.showSigned,    value: "[signed]"}
                                        ])}),
            new SelectFiltering({
                             name: "process",
                             textWidth : "90px",
                             options: [ {name: localization.filterByProcess.showAllProcesses,  value: ""},
                                        {name: localization.filterByProcess.showContractsOnly, value: "contract"},
                                        {name: localization.filterByProcess.showOffersOnly,    value: "offer"},
                                        {name: localization.filterByProcess.showOrdersOnly,    value: "order"}
                                      ]}),
            archive.forCompanyAdmin() ?
              [new SelectAjaxFiltering({
                             name: "sender",
                             textWidth : "90px",
                             text : "sender",
                             optionsURL : "/companyaccounts",
                             defaultName : localization.filterByAuthor.showAnyAuthor,
                             optionsParse: function(resp) {
                                        var options = []
                                        _.each(resp.list, function(l) {
                                          var fields = l.fields;
                                          var id = fields["id"];
                                          var name = fields["fullname"];
                                          if (name == undefined || name == "" || name == " ")
                                            name = fields["email"];
                                          if (fields["activated"])
                                            options.push({name : name , value : id });
                                        });
                                        return options;
                                   }
                 })] : [],
            new IntervalDoubleSelectFiltering({
                             name: "time",
                             textWidth : "90px",
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
            ]);
};
    
window.DocumentsListDefinition = function(archive) { return {
    name : "Documents Table",
    loadOnInit : false,
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Document" },
    sorting: new Sorting({ fields: ["title", "status", "time", "party", "author"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: localization.archive.documents.search}),
    selectfiltering : DocumentSelectsDefinition(archive, true),
    cells : DocumentCellsDefinition(archive),
    actions : [
           new ListAction({
                name : localization.archive.documents.createnew,
                avaible : function() {return true;},
                acceptEmpty : true,
                button: Button.init({
                            size: "tiny",
                            color : "black",
                            width : "110",
                            text: localization.archive.documents.createnew,
                            onClick : function() {
                               // blocking
                               if(archiveBlocking && archiveBlocking.shouldBlockDocs(1)) {
                                 archiveBlocking.createPopup();
                                 return false;
                               }


                              new Submit({
                                
                                

                                method : "POST",
                                url : "/api/frontend/createfromfile",
                                ajax: true,
                                expectedType: 'json',
                                onSend: function() {
                                            LoadingDialog.open();
                                        },
                                ajaxsuccess: function(d) {
                                    if (d != undefined && d.id != undefined) {
                                        window.location.href = "/d/"+d.id;
                                    }
                                    else {
                                        FlashMessages.add({content: localization.couldNotUpload+" "+d, color: "red"});
                                        LoadingDialog.close();
                                        wiz.trigger('change');
                                    }
                                }   
                            }).send();
                          }
                        })
        }),
        new ListAction({
            name :  localization.archive.documents.sendreminder.action,
            emptyMessage :  localization.archive.documents.sendreminder.emptyMessage,
            notAvailableMessage :  localization.archive.documents.sendreminder.notAvailableMessage,
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
            emptyMessage :  localization.archive.documents.cancel.emptyMessage,
            notAvailableMessage :  localization.archive.documents.cancel.notAvailableMessage,
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
            emptyMessage :  localization.archive.documents.cancel.emptyMessage,
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
                 acceptEmpty : true, // We handle in manually
                 onSelect: function(docs){
                        if (docs == undefined || docs.length == 0 ) {
                         FlashMessages.add({color : "red", content : localization.archive.documents.zip.emptyMessage}); 
                         return true;
                        } 
                        if (docs.length == 1) {
                          var url =  "/downloadmainfile/" + docs[0].field("id") + "/" + docs[0].field("title") + ".pdf";
                          window.open(url);
                          return true;
                        } else {  
                          var url =  "/d/zip?"
                          _.each(docs,function(doc){url+=("doccheck="+doc.field("id")+"&")})
                          window.open(url);
                          return true;
                        }
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
                        box.append(description("problem",localization.archive.documents.statusDescription.cancelled));
                        box.append(description("sent",localization.archive.documents.statusDescription.sent));
                        box.append(description("delivered",localization.archive.documents.statusDescription.delivered));
                        box.append(description("read",localization.archive.documents.statusDescription.read));
                        box.append(description("opened",localization.archive.documents.statusDescription.opened));
                        box.append(description("signed",localization.archive.documents.statusDescription.signed));
                        return box;
                    }()
};};


})(window);
