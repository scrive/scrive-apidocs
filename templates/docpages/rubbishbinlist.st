####		
pageRubbishBinList=
$loadingdialog()$
<div> 
    <div class="tab-viewer">
        <div class="tab-header">
        <a class="backicon float-left" href="$linkdoclist$" style="margin-left:-10px;"></a>
        $docListRubbishBinLink()$
        </div>
        $docListTabs()$
    </div>
</div>

<script>
var documentsTable = KontraList().init({
    name : "Trash table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Rubbish" },
    sorting: new Sorting({ fields: ["title", "time", "type"]}),
    paging: new Paging({}),
    textfiltering: new TextFiltering({text: "", infotext: "$_SearchDoc()$"}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: "$_SortTime()$", width:"140px", field:"time"}),
        new Cell({name: "$_SortType()$", width:"120px", field:"type"}),
        new Cell({name: "$_SortDoc()$", width:"400px", field:"title",  special: "link"})
        ],
    options : [{name : "$_restore()$",
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                restore: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_listRestoreDialogHead()$",
                                content: jQuery("<p/>").text("$_listRestoreDialogBody()$")
                              })
                          }
               },
               {name : "$_delete()$",
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                reallydelete: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                              Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_listReallyDeleteDialogHead()$",
                                content: jQuery("<p/>").text("$_listReallyDeleteDialogBody()$")
                              })
                          }
                }
              ]

    })

});


jQuery(function(){
    jQuery(".tab-viewer").append(documentsTable.view.el)
});
</script>
####
