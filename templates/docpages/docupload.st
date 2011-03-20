####    Pages for documents - main part of system
uploadPage=   
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
<table style="width:100%"> <tbody> <tr>
<td>
<div class="signStepsBodyUploadBox"> 
  <form action="/d" method="post" enctype="multipart/form-data"  class="stepForm"> 
    <div>
      <span id="signStepsBodyUploadTextContainer"> 
        <span class="header">Välj fil att ladda upp</span><br/>
        <span class="text">OBS! Endast PDF</span> 
      </span> 
      <br/>
      <div class="signStepsButtonContainer">
        <div class="uploadButton" >  
          <a href="#attachFile"><span class="icon arrowUp"></span></a>
          <a href="#attachFile"> <span class="text">Välj fil</span> </a> 
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf" rel=".filelist" />
        </div>
      </div>
      <div class="filelist" style="padding-left:10px"></div>
    </div>
  </form>
</div> 
</td><td>
<div class="signStepsBodyUploadBox">  
    <div>
      <span id="signStepsBodyUploadTextContainer"> 
        <span class="header">Välj existerande mall</span><br/>
        <span class="text"></span> 
      </span> 
      <br/>
      <div class="signStepsButtonContainer">
        <div class="uploadButton selectTemplateButton" rel=".templatedDialogContainer">  
          <a href="#attachFile"><span class="icon arrowRight"></span></a>
          <a href="#attachFile"> <span class="text">Välj mall</span> </a> 
        </div>
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
    jQuery(".selectTemplateButton").click(function(){getTemplates(this)})
    $else$
    getTemplates(jQuery(".selectTemplateButton"))
    $endif$
    })
 </script>
####