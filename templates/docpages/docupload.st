####    Pages for documents - main part of system
contractuploadname=$_contractuploadname()$
####
offeruploadname=$_offeruploadname()$
####
orderuploadname=$_orderuploadname()$
####
contractuploadprompttext=$_contractuploadprompttext()$
####
offeruploadprompttext=$_offeruploadprompttext()$
####
orderuploadprompttext=$_orderuploadprompttext()$
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
      <span class="step1select text">$_step1select()$</span>
    </div>
    
    <div id="step2select">
      <span class="step2select clickArea"> </span>
      <span class="step2select step2icon"> </span>
      <span class="step2select text">$_step2select()$</span>
    </div>
    
    <div id="step3select">
      <span class="step3select clickArea"> </span>
      <span class="step3select step3icon"> </span>
      <span class="step3select text">$_step3select()$</span>
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
       $_signStepsBodyUploadBoxStep1Info()$</p>
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
  <form action="/d" method="post" enctype="multipart/form-data" > 
    <div class="signStepsBodyUploadBox">
        <input type="hidden" name="doctype" value="$selectedprocess.id$" />
        <span class="header">$_signStepsBodyUploadBoxStep1UpLoadChooseFile()$</span><br/>
        <span class="text">$_signStepsBodyUploadBoxStep1UpLoadOnlyPDF()$</span> 
      <br/>
      <div class="signStepsButtonContainer">
        <a class="btn-small green" >
          <div class="left"> </div>
          <div class="label" style="min-width:100px"> $_signStepsBodyUploadBoxStepButtonChooseFile()$</div>
          <div class="right"> </div>
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf" rel=".filelist" />
        </a>
       </div>
      <div class="filelist" style="padding-left:10px;display:none"></div>
    </div>
  </form>
</td>

<td>
  <div class="signStepsBodyUploadBox $if(showTemplates)$ selected $endif$">  
        <span class="header">$_signStepsBodyUploadBoxStep1ChooseTemplate()$</span><br/>
        <span class="text">$_signStepsBodyUploadBoxStep1TemplateSaved()$</span>
      <br/>
      <div class="signStepsButtonContainer" style="height:34px">
        <a class="btn-small green selectTemplateButton" href="/?doctype=$selectedprocess.id$&amp;showTemplates=YES">  
          <div class="left"> </div>
          <div class="label" style="min-width:100px">$_signStepsBodyUploadBoxStep1ButtonChooseTemplate()$</div>
          <div class="right"> </div>
        </a>
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
