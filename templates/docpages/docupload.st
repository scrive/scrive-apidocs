###
createFromTemplatePage=

<div class="mainContainer" >
<div class="body-container">
<div class="blocking-box"></div>
<script>
jQuery(function() {
  BlockingInfo.show('.blocking-box');
  mixpanel.register({Context : 'Create from template'});
  mixpanel.track('View Create From Template');
});
jQuery(".body-container").append(new CreateFromTemplate().el());
</script>
</div>
</div>
####
newDocumentTitle=$_newDocumentTitle()$
####
newTemplateTitle=$_newTemplateTitle()$
####
