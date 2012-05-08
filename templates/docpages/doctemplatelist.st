####
pageTemplatesList=
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
    name : "Templatesarchive table",
    schema: new Schema({
    url: "/docs",
    extraParams : { documentType : "Template" },
    sorting: new Sorting({ fields: ["title", "time", "process"]}),
    paging: new Paging({}),
    filtering: new Filtering({text: "", infotext: "$_SearchDoc()$"}),
    cells : [
        new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
        new Cell({name: "$_SortTime()$", width:"140px", field:"time"}),
        new Cell({name: "$_SortType()$", width:"120px", field:"process"}),
        new Cell({name: "$_SortTemplate()$", width:"400px", field:"title",  special: "link"}),
        new Cell({name: "", width:"100px", field:"shared", special: "rendered",
                  rendering: function(shared) {
                         var res = jQuery("<p/>")
                         if (shared == "True")
                          return res.text("$_shared()$");
                         return res;
                  }})
        ],
    options : [{name : "$_share()$",
                onSelect: function(docs){
                             var submit = new Submit({
                                                url: "$currentlink$",
                                                method: "POST",
                                                share: "true",
                                                doccheck: _.map(docs, function(doc){return doc.field("id");})
                                          });
                            Confirmation.popup({
                                submit: submit,
                                acceptText: "$_ok()$",
                                rejectText: "$_cancel()$",
                                title: "$_listShareDialogHead()$",
                                content: jQuery("<p/>").text("$_listShareDialogBody()$")
                              })
                          }
               },
               {name : "$_delete()$",
                onSelect: function(docs){
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
                               confirmtext.append(docs.length + (" $_docListTabsTemplates()$").toLowerCase());
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
                }
              ]

    }),
    headerExtras: UploadButton.init({
                    size: "tiny",
                    width : "130",
                    text: "$_createNew()$",
                    name : "doc",
                    submitOnUpload : true,
                    submit: new Submit({
                          method : "POST",
                          url : "$currentlink$"
                        })
                    }).input()
});


jQuery(function(){
    jQuery(".tab-viewer").append(documentsTable.view.el)
});
</script>
####
