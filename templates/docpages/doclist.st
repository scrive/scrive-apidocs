####
pageDocumentsList=
$loadingdialog()$
<div>
	<div class="tab-viewer">
	  <div class="tab-header">
    $docListRubbishBinLink()$
	  </div>
	  $docListTabs()$
    </div>

</div>
<script>
var documentsTable = KontraList().init({
    name : "Documents Table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Document" },
    sorting: new Sorting({ fields: ["title", "status", "time", "party", "author"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: "$_SearchDoc()$"}),
    selectfiltering : [
        new SelectFiltering({description: localization.filterByProcess.showAllProcesses, name: "process",
                             options: [ {name: localization.filterByProcess.showContractsOnly, value: "contract"},
                                        {name: localization.filterByProcess.showOffersOnly,    value: "offer"},
                                        {name: localization.filterByProcess.showOrdersOnly,    value: "order"}
                                      ]})
        // new SelectFiltering({description: "All authors", name: "owner", 
        //                     options : [ {name: "Only mine", value : "user"}
        //                               ]}),
        ],
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: "$_SortStatus()$", width:"40px", field:"status", 
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
        new Cell({name: "$_SortTime()$", width:"116px", field:"time"}),
        new Cell({name: "$_SortSender()$", width:"110px", field:"author",  special: "link"}),
        new Cell({width:"5px" }),
        new Cell({name: "$_SortParty()$", width:"200px", field:"party", special: "expandable", subfield : "name"}),
        new Cell({width:"5px" }),
        new Cell({name: "$_SortDoc()$", width:"250px", field:"title",  special: "link"}),
        new Cell({width:"5px" }),
        new Cell({name: "$_SortType()$", width:"40px", field:"process",
                  rendering: function(value, _idx, _model) {
                      var txt = "";
                      if( localization.process[value] !== undefined ) {
                          txt = localization.process[value].shortName;
                      }
                      return jQuery("<span />").text(txt);
                    }
                 })
        ],
    options : [{name : "$_SendReminder()$",
                onSelect: function(docs){
                            allSendOrOpenSelected = _.all(docs, function(doc) {
                                                return doc.field("status") == "sent"      ||
                                                       doc.field("status") == "delivered" ||
                                                       doc.field("status") == "read"      ||
                                                       doc.field("status") == "opened"
                                            })
                             if (!allSendOrOpenSelected) {
                                FlashMessages.add({content: localization.cantSendReminder('$_docListTabsDocs()$'.toLowerCase()), color: "red"});
                                return;
                             }
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remind: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var content = jQuery("<p/>");
                             if (docs.length == 1) {
                               content.append("$_ContractReminderBodySingel()$ ");
                               content.append(jQuery("<strong/>").text(docs[0].field("title")));
                               content.append("?");
                             } else {
                               content.text("$_ContractReminderBodyMulti()$ "+ docs.length + (" $_docListTabsDocs()$?").toLowerCase());
                             }
                             Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_SendReminder()$",
                                content: content
                              })
                          }
               },
               {name : "$_delete()$",
                onSelect: function(docs){
                            anySendOrOpenSelected = _.any(docs, function(doc) {
                                                return doc.field("status") == "sent"      ||
                                                       doc.field("status") == "delivered" ||
                                                       doc.field("status") == "read"      ||
                                                       doc.field("status") == "opened"    ||
                                                       doc.field("anyinvitationundelivered") == "True"
                                            })
                             if (anySendOrOpenSelected) {
                                FlashMessages.add({content: "$_CantRemoveActiveDocuments()$", color: "red"});
                                return;
                             }
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                remove: "true",
                                                archive: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                             var confirmtext = jQuery("<p/>").append("$_listLocalDialogsBody()$ ");
                             var label = jQuery("<strong/>");
                             if (docs.length == 1) {
                               confirmtext.append(jQuery("<strong/>").text(docs[0].field("title")));
                             } else {
                               confirmtext.append(docs.length + (" $_docListTabsDocs()$").toLowerCase());
                             }
                             confirmtext.append("?");
                             Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_delete()$",
                                content: confirmtext
                              });
                          }
                },
               {name : "$_csv()$",
                onSelect: function(){
                        var url =  documentsTable.schema.url() + "?"
                        var params = documentsTable.schema.getSchemaUrlParams();
                        params.format = "csv";
                        _.each(params,function(a,b){url+=(b+"="+a+"&")})
                        window.location = url;
                }
               } 
              ]
    }),
    bottomExtras : jQuery("<div class='table-statuses'>" +
                    "<div class='icon status draft float-left'></div><div class='float-left'>$_tablestatusesDraft()$&nbsp;</div>" +
                    "<div class='icon status cancelled float-left'></div><div class='float-left'>$_tablestatusesCancelled()$&nbsp;</div>" +
                    "<div class='icon status sent float-left'></div><div class='float-left'>$_tablestatusesSent()$&nbsp;</div>" +
                    "<div class='icon status delivered float-left'></div><div class='float-left'>$_tablestatusesDelivered()$&nbsp;</div>" +
                    "<div class='icon status read float-left'></div><div class='float-left'>$_tablestatusesEmailOpened()$&nbsp;</div>" +
                    "<div class='icon status opened float-left'></div><div class='float-left'>$_tablestatusesViewedOnline()$&nbsp;</div>" +
                    "<div class='icon status signed float-left'></div><div class='float-left'>$_tablestatusesSigned()$&nbsp;</div>" +
                    "</div>")
});

jQuery(function(){
    jQuery(".tab-viewer").append(documentsTable.view.el)
});
</script>
####
