####		
pageOffersList=
$loadingdialog()$
<div>
  <div id="tab-viewer">
	<div class="tab-header">
      $docListRubbishBinLink()$
	</div>
	$docListTabs()$
  </div>
</div>

<script>
  var documentsTable = KontraList.init({
  name : "Offers Table",
  schema: new Schema({
  url: "/docs",
  extraParams : { documentType : "Offer" },
  sorting: new Sorting({ fields: ["title", "status", "time","partner", "partnercomp", "author"]}),
    paging: new Paging({}),
    filtering: new Filtering({text: "", infotext: "$_SearchDoc()$"}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: "$_SortStatus()$", width:"65px", field:"status", special: "rendered",
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
        new Cell({name: "$_SortTime()$", width:"130px", field:"time"}),
        new Cell({name: "$_SortReciever()$", width:"130px", field:"partner"}),
        new Cell({name: "$_SortCompany()$", width:"130px", field:"partnercomp",  special: "link"}),
        new Cell({name: "$_SortSender()$", width:"130px", field:"author",  special: "link"}),
        new Cell({name: "$_SortDoc()$", width:"227px", field:"title",  special: "link"})
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
                                FlashMessages.add({content: localization.cantSendReminder('$_docListTabsOffers()$'.toLowerCase()), color: "red"});
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
                               text = "$_OfferReminderBodySingel()$ "+ docs[0].field("title") + "."
                             else
                               text = "$_OfferReminderBodyMulti()$ "+ docs.length + (" $_docListTabsOffers()$.").toLowerCase() + " $_OfferReminderBodyMulti2()$";
                             var content = jQuery("<p/>").text(text);
                             Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_OfferReminderHead()$",
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
                                FlashMessages.add({content: "$_CantRemoveActiveOffers()$", color: "red"});
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
                               text += docs.length + (" $_docListTabsOffers()$.").toLowerCase();
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
              ]
    }),
    bottomExtras : jQuery("<div class='table-statuses'>" +
                    "<div class='icon status draft float-left'></div><div class='float-left'>$_tablestatusesDraft()$&nbsp;</div>" +
                    "<div class='icon status cancelled float-left'></div><div class='float-left'>$_tablestatusesCancelled()$&nbsp;</div>" +
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
