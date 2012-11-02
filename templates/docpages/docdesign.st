#####
pageDocumentDesign=<html></html>
<script>
jQuery("body").addClass("designview");
jQuery(function(){
  jQuery(".body-container").replaceWith(new KontraDesignDocument({
                        id:"$documentid$"
                  }).el());
});
var designViewBlocking = null;
jQuery(function() {
  designViewBlocking = Blocking();
});
</script>
###
