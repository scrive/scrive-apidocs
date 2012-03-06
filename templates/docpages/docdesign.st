#####
pageDocumentDesign=
<script>

jQuery(function(){

  var jsuploadtext = jQuery("#jsuploadtext");
  var jsuploadbutton = jQuery("#jsuploadbutton");
  var jschoosetemplate = jQuery("#jschoosetemplate");

  var docview = KontraDesignDocument.init({
    id:"$documentid$",
    basic : $if(isbasic)$ true $else$ false $endif$

  });
  jQuery(".body-container").replaceWith(docview.view.el);
  jQuery(".body-container").append(jsuploadtext).append(jsuploadbutton).append(jschoosetemplate);
});
</script>

<script type="text/template" id="jsuploadtext">
  <td> 
    <div class="signStepsBodyUploadBox" style="border:0px;text-align:left;width: 250px;font-weight:bold"> 
      <p class="text" style="padding-top:30px">
      $_uploadNewFile()$
      </p>
    </div>
  </td>
</script>

<script type="text/template" id="jsuploadbutton">
  <td class="jsformbox">
    <form action="/api/document/$documentid$" method="post" enctype="multipart/form-data" class="jsuploadform"> 
      <div class="signStepsBodyUploadBox">
        <span class="header">$_signStepsBodyUploadBoxStep1UpLoadChooseFile()$</span><br/>
        <span class="text">$_onlyPDF()$</span>
        <br/>
        <div class="signStepsButtonContainer"></div>
      </div>
    </form>
  </td>
</script>

<script type="text/template" id="jschoosetemplate">
  <td>
    <div class="signStepsBodyUploadBox $if(showTemplates)$ selected $endif$">  
      <span class="header">$_signStepsBodyUploadBoxStep1ChooseTemplate()$</span><br/>
      <span class="text">$_signStepsBodyUploadBoxStep1TemplateSaved()$</span>
      <br/>
      <div class="signStepsButtonContainer" style="height:34px">
        <a class="btn-small green selectTemplateButton" href="$linkupload$?doctype=$selectedprocess.id$&amp;showTemplates=YES">  
          <div class="left"> </div>
          <div class="label" style="width:100px">$_signStepsBodyUploadBoxStep1ButtonChooseTemplate()$</div>
          <div class="right"> </div>
        </a>
      </div>
    </div>
  </td>
</script>

###
###
contracttitle=$_contracttitle()$
###
offertitle=$_offertitle()$
###
ordertitle=$_ordertitle()$
###
contractname=$_contractname()$
###
offername=$_offername()$
###
ordername=$_ordername()$
###
contractstep1text=$_contractstep1text()$
####
offerstep1text=$_offerstep1text()$
###
orderstep1text=$_orderstep1text()$
###
contractexpirywarntext=$_contractexpirywarntext()$
####
offerexpirywarntext=$_offerexpirywarntext()$
####
orderexpirywarntext=$_orderexpirywarntext()$
####
contractsendtext=$_contractsendtext()$
####
offersendtext=$_offersendtext()$
####
ordersendtext=$_ordersendtext()$
####
contractconfirmsendtitle=$_contractconfirmsendtitle()$
####
offerconfirmsendtitle=$_offerconfirmsendtitle()$
####
orderconfirmsendtitle=$_orderconfirmsendtitle()$
####
contractconfirmsendtext=$_contractconfirmsendtext()$
####
offerconfirmsendtext=$_offerconfirmsendtext()$
####
orderconfirmsendtext=$_orderconfirmsendtext()$
####
contractexpirytext=$_contractexpirytext()$
####
offerexpirytext=$_offerexpirytext()$
####
orderexpirytext=$_orderexpirytext()$
####
csvsendoutsignatoryattachmentstring=Massutskick
####
