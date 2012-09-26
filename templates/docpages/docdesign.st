#####
pageDocumentDesign=
<script>
jQuery(function(){
  var docview = KontraDesignDocument.init({
    id:"$documentid$",
    basic : $if(isbasic)$ true $else$ false $endif$
  });
  jQuery(".body-container").replaceWith(docview.view.el);
});
</script>
###
