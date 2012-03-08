pagePadDeviceArchive=
<div>
  <div class="tab-viewer paddevicearchive">
        <div class="tab-header">
      $docListRubbishBinLink()$
        </div>
        $docListTabs()$
  </div>
</div>
<script>

var padTable = KontraList().init({
    name : "Pad Table",
    headerExtras: Button.init({
                    text: "Open pad view",
                    size: "tiny",
                    color: "blue",
                    onClick : function() {
                       window.location = "/padqueue";
                    }
            }).input(),
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Pad" },
    sorting: new Sorting({ fields: ["status", "time", "title","party"]}),
    paging: new Paging({}),
    filtering: new Filtering({text: "", infotext: "$_SearchDoc()$"}),
    cells : [
         new Cell({name: "$_SortStatus()$", width:"65px", field:"status", special: "rendered",
                 rendering: function(status,idx,listobject) {
                        return jQuery("<div class='icon status "+status+"'></div>")
                 }
        }),
        new Cell({name: "$_SortTime()$", width:"140px", field:"time" , special: "rendered",
                 rendering: function(time,idx,listobject) {
                    var tbox = jQuery("<div/>");
                    if (idx == undefined) tbox.text(time);
                    return tbox;
                 }
        }),
        new Cell({name: "$_SortDoc()$", width:"240px", field:"title",  special: "expandable"}),
        new Cell({name: "$_SortParty()$", width:"240px", field:"party", special: "expandable", subfield : "name"}),
        new Cell({name: "Pad", width:"50px", field:"id",  special: "rendered",
                 rendering: function(id,idx,listobject) {
                    var icon = jQuery("<div class='statusIcon'/>");
                    if(listobject.field("inpadqueue") == "true" && idx == undefined) {
                        icon.addClass("inqueuestatus");
                        ToolTip.set({on: icon, tip : "This document is send to pad device for signing"});
                    }
                    else if (idx == undefined)
                       {
                         icon.addClass("notinqueuestatus");
                         ToolTip.set({on: icon, tip : "This document is waiting to be send to pad device"});
                       }
                    else if (listobject.field("inpadqueue") == "true" && listobject.subfield(idx,"inpadqueue") == "true")
                       {
                         icon.addClass("inqueuestatus");
                         ToolTip.set({on: icon, tip : "This signatory can sign on pad device"});
                       } 
                    else
                       {
                         icon.addClass("notinqueuestatus");
                         //ToolTip.set({on: icon, tip : "This signatory has not signed and document is not waiting for him in queue"});
                       }
                    return icon;
                 }}),
       new Cell({name: "Action", width:"50px", field:"id", substyle: "", special: "rendered",
                 rendering: function(id,idx,listobject) {
                    var actionIcon = jQuery("<a class='actionIcon'/>");
                    if(idx == undefined)
                        {
                            actionIcon.addClass("gotodocument");
                            ToolTip.set({on: actionIcon, tip : "See this document"});
                            actionIcon.attr("href","/d/"+listobject.field("id"));

                        }
                    else if (listobject.field("inpadqueue") == "true" && listobject.subfield(idx,"inpadqueue") == "true")
                        {
                            actionIcon.addClass("removefromqueue");
                            ToolTip.set({on: actionIcon, tip : "Remove from pad device"});
                            actionIcon.click(function() {
                                new Submit({
                                    url: "/padqueue/clear" ,
                                    method: "POST"
                                }).sendAjax(function() { LoadingDialog.close(); padTable.recall(); });
                            });
                        }
                    else if (listobject.subfield(idx,"author") != "true")
                        {
                            actionIcon.addClass("addtoqueue");
                            ToolTip.set({on: actionIcon, tip : "Send to pad device"});
                            actionIcon.click(function() {
                            LoadingDialog.open();
                            new Submit({
                                    url: "/padqueue/add/"+ listobject.field("id") + "/" +  listobject.subfield(idx,"id") ,
                                    method: "POST"
                                }).sendAjax(function() { LoadingDialog.close(); padTable.recall(); });
                            return false;    
                            });
                        }

                    return actionIcon;
                 }}),
        ]
    })
});

jQuery(function(){
    jQuery(".tab-viewer").append(padTable.view.el)
});
</script>
####
padQueueCurrentPage=
<div class="padview"></div>
<script>
var padTable = KontraPad.init({ });

jQuery(function(){
    jQuery(".padview").append(padTable.view.el)
});

</script>