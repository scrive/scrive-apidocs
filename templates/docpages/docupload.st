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
<div class="tab-viewer">
<div id="signStepsContainer" class="uploading">
  <script type="text/javascript">
    jQuery(UploadProcessMain);
  </script>
  <div id="signStepsTitleRow">
    <span id="signStepsTitleRowTextContainer">
    </span>

  </div>

    <ul class="tabs">
       <li class="active">
            <span class="numbericon1"></span><a href="#">$_step1select()$</a>
      </li>
      <li>
            <span class="numbericon2"></span><a href="#">$_step2selectIfisbasic()$</a>
      </li>
  </ul>
  <div class="signStepsBody">

<!-- MAIN FORM STARTS HERE -->
<!-- Templates -->

<script type="text/template" id="jschooseprocesstext">
  <td>
    <div class="signStepsBodyUploadBox" style="border:0px;text-align:left;width: 250px;font-weight:bold">
      <p class="text" style="padding-top:40px">
        $_signStepsBodyUploadBoxStep1Info()$</p>
    </div>
  </td>
</script>

<script type="text/template" id="jschooseprocess">
  <td>
    <div class="documentTypeBox">
      <a class="documenticon" href="#"></a>
    </div>
  </td>
</script>

<script type="text/template" id="jsuploadtext">
  <td>
    <div class="signStepsBodyUploadBox" style="border:0px;text-align:left;width: 250px;font-weight:bold">
      <p class="text jsprompt" style="padding-top:30px">
      </p>
      <p style="padding-top:25px;">
        <a class="backicon float-left jswizardback" href="#"></a> &nbsp;$_GoBack()$
      </p>
    </div>
  </td>
</script>

<script type="text/template" id="jsuploadbutton">
  <td class="jsformbox">
    <form action="/api/document" method="post" enctype="multipart/form-data" class="jsuploadform">
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

<table style="width:100%" >
  <tbody>
  </tbody>
</table>
<!-- MAIN FROM END HERE -->
</div>
</div>
</div>
####
