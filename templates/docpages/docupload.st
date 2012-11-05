###
createFromTemplatePage=
<script>
jQuery(".mainContainer").append(new CreateFromTemplate().el());
</script>
<script>
var uploadBlocking;
jQuery(function() {
  uploadBlocking = Blocking();
  uploadBlocking.show('.blocking-box');
});
</script>
####
newDocumentTitle=$_newDocumentTitle()$
####
newTemplateTitle=$_newTemplateTitle()$
####
