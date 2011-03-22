####    Pages for documents - main part of system
uploadPage=   
$loadingdialog()$
<div id="signStepsContainer">
  
  <div id="signStepsTitleRow">
    <span id="signStepsTitleRowTextContainer">
    </span>

  </div>


  <div id="signStepsMenu">
    <div id="step1select" class="current">
      <span class="step1select clickArea"> </span>
      <span class="step1select step1icon"> </span>
      <span class="step1select text">Välj dokument</span>
    </div>
    
    <div id="step2select">
      <span class="step2select clickArea"> </span>
      <span class="step2select step2icon"> </span>
      <span class="step2select text">Fyll i uppgifter</span>
    </div>
    
    <div id="step3select">
      <span class="step3select clickArea"> </span>
      <span class="step3select step3icon"> </span>
      <span class="step3select text">Underteckna och skicka!</span>
    </div>
    
    <div id="signStepsMenuEmptyStep"></div>
  </div>
  
  <div id="signStepsBody"> 
 
<!-- MAIN FORM STARTS HERE -->
<table style="width:100%"> <tbody> <tr valign="top">
<td> <div class="signStepsBodyUploadBox"> 
    <p class="text">
    Välj det dokument som du vill fylla i och skicka till din motpart för att skriva på.
    Du väljer ett dokument från din dator eller från existerande mallar.
    <p/>
 </div>
</td>
<td>
<div class="signStepsBodyUploadBox"> 
  <form action="/d" method="post" enctype="multipart/form-data" > 
    <div>
        <span class="header">Välj fil att ladda upp</span><br/>
        <span class="text">OBS! Endast PDF</span> 
      <br/>
      <div class="signStepsButtonContainer">
        <a class="btn-small green" style="overflow:hidden;height:34px;width:114px" >  
          <div class="left"> </div>
          <div class="label" style="width:80px"> Välj fil </div> 
          <div class="right"> </div>
          <input class="multiFileInput submitOnUpload" style="top:-40px;right:0px" maxlength="1" type="file" name="doc" accept="application/pdf" rel=".filelist" />
        </a>
       </div>
      <div class="filelist" style="padding-left:10px"></div>
    </div>
  </form>
</div> 
</td><td>
<div class="signStepsBodyUploadBox">  
    <div>
        <span class="header">Välj existerande mall</span><br/>
        <span class="text"></span> 
      <br/>
      <div class="signStepsButtonContainer" style="height:34px">
        <a class="btn-small green selectTemplateButton" rel=".templatedDialogContainer">  
          <div class="left"> </div>
          <div class="label" style="width:80px"> Välj mall </div> 
          <div class="right"> </div>
        </a>
      </div>
    </div>   
</div>
</td> </tr></tbody></table>
<!-- MAIN FROM END HERE --> 
</div>
  <div class="templatedDialogContainer" style="display:none">
        <div class='waiting4data'></div></div>
 </div>     
</div>     
 <script>
    function getTemplates(curr){
        var location = jQuery(jQuery(curr).attr("rel"));
        var wheretoputdata = jQuery(".waiting4data",location);
        location.show();
        jQuery.get("$templateslink:noescape()$" ,function(data){
                 wheretoputdata.replaceWith(data);
                 enableInfoTextOnce(location);
                 jQuery('a.submit').altSubmit();
        })
    }

   safeReady(function(){
    $if(!showTemplates)$
    jQuery(".selectTemplateButton").click(function(){
        addFlashMessage("Klicka på den mall du vill välja för att starta processen","green");
        getTemplates(this);
    })
    $else$
    getTemplates(jQuery(".selectTemplateButton"))
    $endif$
    })
 </script>
####
