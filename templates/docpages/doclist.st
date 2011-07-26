####		
pageContractsList=
$loadingdialog()$
<div>
	<div id="tab-viewer">
	  <div class="tab-header">

	  </div>
	  $docListTabs()$
    </div>

</div>
<script>
var documentsTable = KontraList.init({
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Contract" },
    sorting: new Sorting({ fields: ["title", "status", "time","partner"]}),
    paging: new Paging({}),
    filtering: new Filtering({text: "", infotext: "$_SearchDoc()$"}),
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
                             var text;
                             if (docs.length == 1)
                               text = "$_ReminderBodyIfSingel()$ "+ docs[0].field("title") + "."
                             else
                               text = "$_ReminderBodyIfMulti()$ "+ docs.length + (" $_docListTabsDocs()$.").toLowerCase() + " $_ReminderBodyIfMulti2()$";
                             var content = jQuery("<p/>").text(text);
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
                                                       doc.field("status") == "opened"    
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
                             var text = "$_listLocalDialogsBody()$"
                             if (docs.length == 1)
                               text += docs[0].field("title") + "."
                             else
                               text += docs.length + (" $_docListTabsDocs()$.").toLowerCase();
                             var content = jQuery("<p/>").text(text);
                             Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_delete()$",
                                content: content
                              })
                          }
                }
              ],
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: "$_SortStatus()$", width:"65px", field:"status", special: "rendered",
                 rendering: function(status,mainrow,listobject) {
                    var icon = jQuery("<div class='icon status "+status+"'></div>")
                    if (listobject.field("anyinvitationundelivered") == "true" && mainrow == true)
                       icon.add("<span style='color:#000000;'>!</span>");

                    var tip = jQuery("<div id='tooltip-"+status+"'> <div class='icon status "+status+"'></div><p>"+
                                      localization.statusToolTip[status]+"</p></div>");
                    ToolTip.set({
                        on: icon,
                        tip: tip
                    })
                    return icon;
                 }
        }),
        new Cell({name: "$_SortTime()$", width:"140px", field:"time"}),
        new Cell({name: "$_SortParty()$", width:"300px", field:"partner", special: "expandable"}),
        new Cell({name: "$_SortDoc()$", width:"300px", field:"title",  special: "link"})
        ]
    }),
    /* headerExtras : Button.init({color:"red", size: "tiny", text: "Kill me"}).input(), */
    bottomExtras : jQuery("<div class='table-statuses'>" +
                    "<div class='icon status draft float-left'></div><div class='float-left'>$_tablestatusesDraft()$&nbsp;</div>" +
                    "<div class='icon status cancelled float-left'></div><div class='float-left'>$_tablestatusesCandelled()$&nbsp;</div>" +
                    "<div class='icon status sent float-left'></div><div class='float-left'>$_tablestatusesSent()$&nbsp;</div>" +
                    "<div class='icon status delivered float-left'></div><div class='float-left'>$_tablestatusesDelivered()$&nbsp;</div>" +
                    "<div class='icon status read float-left'></div><div class='float-left'>$_tablestatusesEmailOpened()$&nbsp;</div>" +
                    "<div class='icon status opened float-left'></div><div class='float-left'>$_tablestatusesViewedOnline()$&nbsp;</div>" +
                    "<div class='icon status signed float-left'></div><div class='float-left'>$_tablestatusesConfirmed()$&nbsp;</div>" +
                    "</div>")
});

jQuery(function(){
    jQuery("#tab-viewer").append(documentsTable.view.el)
});
</script>
####
