#####
pageDocumentDesign=
<div class="mainContainer" >
<div class="body-container">
<script>
jQuery("body").addClass("designview");
jQuery(function(){
  jQuery(".body-container").replaceWith(new DesignView({
                        id:"$documentid$"
                  }).el());
});
  mixpanel.register({DocumentID: "$documentid$", Context: "design view"});
  mixpanel.track('View Design View');
</script>
</div>
</div>
###
