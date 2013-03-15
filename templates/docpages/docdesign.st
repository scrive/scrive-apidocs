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
  jQuery(".body-container").replaceWith(dv.el());
  dv.finish(); // need to call finish after it is inserted into dom
});
  mixpanel.register({DocumentID: "$documentid$", Context: "design view"});
  mixpanel.track('View Design View');
</script>
</div>
</div>
###
