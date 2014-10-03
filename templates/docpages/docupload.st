###
createFromTemplatePage=

<div class="mainContainer" >
<div class="body-container">
<div class="blocking-box"></div>
<div class='place-for-list'></div>
<script>
 require(['jquery','React','createfromtemplate/createfromtemplate', 'legacy_code'], function(jQuery,React,CreateFromTemplate) {
jQuery(function() {
  mixpanel.register({Context : 'Create from template'});
  mixpanel.track('View Create From Template');
});


 React.renderComponent(
      CreateFromTemplate({
      })
    , jQuery(".place-for-list")[0]
);


//jQuery(".body-container").append(new CreateFromTemplate().el());


});
</script>
</div>
</div>
####
newDocumentTitle=$_newDocumentTitle()$
####
newTemplateTitle=$_newTemplateTitle()$
####
