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
            <span class="numbericon2"></span><a href="#">$_step2selectIfElse()$</a>
      </li>
      <li>
            <span class="numbericon2"></span><a href="#">$_step3selectIfNotTemplate()$</a>
      </li>
  </ul>
  <div class="signStepsBody">

<table style="width:100%" >
  <tbody>
  </tbody>
</table>
<!-- MAIN FROM END HERE -->
</div>
</div>
</div>
####
