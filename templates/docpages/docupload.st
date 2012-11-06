###
createFromTemplatePage=
<div class="blocking-box"></div>
<script>
jQuery(".mainContainer").append(new CreateFromTemplate().el());
</script>
<script>
var BlockingInfo = Blocking();
jQuery(function() {
  BlockingInfo.show('.blocking-box');
});
</script>
####
newDocumentTitle=$_newDocumentTitle()$
####
newTemplateTitle=$_newTemplateTitle()$
####
