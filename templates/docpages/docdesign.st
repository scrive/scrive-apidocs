#####
pageDocumentDesign=<html></html>
<script>
jQuery("body").addClass("designview");
jQuery(function(){
  jQuery(".body-container").replaceWith(new DesignView({
                        id:"$documentid$"
                  }).el());
});
</script>
###
