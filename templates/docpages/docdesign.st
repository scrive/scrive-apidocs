#####
pageDocumentDesign=
<div class="mainContainer" >
<div class="body-container">
<script>
jQuery("body").addClass("designview");
jQuery(function(){
  var dv = new DesignView({
                        id:"$documentid$"
                  });

  jQuery(".body-container").replaceWith(
    jQuery('<div />').append(BlockingInfo.el())
                     .append(dv.el())
                     .children()
  );
  dv.afterInsert();
});
  mixpanel.register({DocumentID: "$documentid$", Context: "design view"});
  mixpanel.track('View Design View');

</script>
</div>
</div>
###
