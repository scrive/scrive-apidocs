####    Pages for documents - main part of system
uploadPage=
<div class='blocking-box'></div>
<div class="upload-view"></div>
<script type="text/javascript">
    var upm = new UploadProcessMain();
    jQuery('.upload-view').append(jQuery(upm.el()).addClass('signStepsBody'));
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
