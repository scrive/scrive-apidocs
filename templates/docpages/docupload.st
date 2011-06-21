####    Pages for documents - main part of system
contractuploadname=AVTAL
####
offeruploadname=OFFERT
####
orderuploadname=ORDER-BEKRÄFTELSE
####
contractuploadprompttext=Välj det dokument som du vill skicka till din motpart för påskrift.
####
offeruploadprompttext=Välj den offert som du vill skicka till din motpart.
####
orderuploadprompttext=Välj den orderbekräftelse som du vill skicka till.
####
uploadPage=   
$loadingdialog()$
<div id="signStepsContainer">
  
  <div id="signStepsTitleRow">
    <span id="signStepsTitleRowTextContainer">
    </span>

  </div>


  <div id="signStepsMenu" class="signStepsMenu">
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
$if(!isprocessselected)$
<table style="width:100%" > <tbody> <tr valign="top">
 <td>
   <div class="signStepsBodyUploadBox" style="border:0px;text-align:left;width: 250px;font-weight:bold"> 
    <p class="text" style="padding-top:40px">
       Vilken typ av dokument vill du skicka? Klicka på dokumentsymbolen för att gå vidare.
    </p>
   </div>
 </td>
  $processes:{process |
  <td>
    <div class="documentTypeBox $if(process.selected)$selected$endif$"> 
       <a class="documenticon" href="/?doctype=$process.id$">$process.name$</a>    
    </div>
  </td>
  }$
</tr></tbody></table>
<!-- MAIN FROM END HERE --> 

<!-- START: Form to select new upload or template  -->
$elseif(!showTemplates)$
<div class="postSignStepBody">
<table style="width:100%"> <tbody> <tr valign="top">
<td> <div class="signStepsBodyUploadBox" style="border:0px;text-align:left;width: 250px;font-weight:bold"> 
    <p class="text" style="padding-top:30px">
        $selectedprocess.uploadprompttext$
    </p>
    <a class="backicon float-left" href="/"></a>
 </div>
</td>
<td>
<div class="signStepsBodyUploadBox"> 
  <form action="/d" method="post" enctype="multipart/form-data" > 
    <div>
        <input type="hidden" name="doctype" value="$selectedprocess.id$" />
        <span class="header">Välj fil från dator</span><br/>
        <span class="text">OBS! Endast PDF</span> 
      <br/>
      <div class="signStepsButtonContainer">
        <a class="btn-small green" style="overflow:hidden;height:34px;width:114px" >  
          <div class="left"> </div>
          <div class="label" style="width:80px"> Välj fil </div> 
          <div class="right"> </div>
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf" rel=".filelist" />
        </a>
       </div>
      <div class="filelist" style="padding-left:10px;display:none"></div>
    </div>
  </form>
</div> 
</td>

<td>
<div class="signStepsBodyUploadBox $if(showTemplates)$ selected $endif$">  
    <div>
        <span class="header">Välj mall</span><br/>
        <span class="text">Sparad på SkrivaPå</span> 
      <br/>
      <div class="signStepsButtonContainer" style="height:34px">
        <a class="btn-small green selectTemplateButton" href="/?doctype=$selectedprocess.id$&amp;showTemplates=YES">  
          <div class="left"> </div>
          <div class="label" style="width:80px"> Välj mall </div> 
          <div class="right"> </div>
        </a>
      </div>
    </div>   
</div>
</td> </tr></tbody></table>
</div>
<!-- END: Form to select new upload or template -->
$else$

<div class="templatedDialogContainer" style="display:none">
        <div class='waiting4data'></div>
</div>
       
 <script>
   safeReady(function(){
      var location = jQuery(".templatedDialogContainer");
        var wheretoputdata = jQuery(".waiting4data",location);
        location.show();
        jQuery.get("$templateslink:noescape()$" ,function(data){
                 wheretoputdata.replaceWith(data);
                 enableInfoTextOnce(location);
                 jQuery('a.submit').altSubmit();
        })
    })
 </script>

$endif$
</div>
</div>
####
