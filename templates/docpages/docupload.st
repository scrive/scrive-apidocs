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
      <span class="step1select text">Ladda upp dokument</span>
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
 

<div id="signStepsBodyUploadBox"> 
  <form action="/d" method="post" enctype="multipart/form-data"
        class="stepForm"> 
    <div>
      <span id="signStepsBodyUploadTextContainer"> 
        <span class="header">Välj fil att ladda upp</span><br/>
        <span class="text">OBS! Endast PDF</span> 
      </span> 
      <br/>
      <div id="signStepsButtonContainer">
        <div class="uploadButton" >  
          <a href="#attachFile"><span class="icon arrowUp"></span></a>
          <a href="#attachFile"> <span class="text">Ladda upp fil</span> </a> 
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf"
                 style="position:relative;z-index:10;cursor:pointer;float:right;top:-15;left:10;font-size:xx-large;cursor:pointer;cursor:hand
                        -ms-filter: 'progid:DXImageTransform.Microsoft.Alpha(Opacity=0)';
                        filter: progid:DXImageTransform.Microsoft.Alpha(Opacity=0);
                        opacity: 0;" 
                 rel=".filelist" />
        </div>
      </div>
      <div class="filelist" style="padding-left:10px"></div>
    </div>
  </form>
</div> 



  </div>
</div>            
####