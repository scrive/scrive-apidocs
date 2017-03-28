####
showAccount =
<div class="mainContainer" >
  <div class="body-container">
    <div class='blocking-box'></div>
    <div class='archive'></div>
    <script>
    var fromTemplate = {
      companyAdmin     : $if(companyAdmin)$true$else$false$endif$
    };
    </script>
    <script src="$cdnbaseurl$/compiled/all/account-$versioncode$.js" crossorigin="anonymous"></script>
  </div>
</div>

#####
statsOrgTotal=$_statsOrgTotal()$
####
pageDoYouWantToBeCompanyAccount=
<div class="mainContainer" >
  <div class="body-container">
  </div>
</div>
<script>
  var fromTemplate = {
    title: "$_modalDoYouWantToBeCompanyAccountHeader()$",
    acceptButtonText: "$_modalDoYouWantToBeCompanyAccountFooterAccept()$",
    body: "$_modalDoYouWantToBeCompanyAccountBody()$"
  };
</script>
<script src="$cdnbaseurl$/compiled/all/company_account-$versioncode$.js" crossorigin="anonymous"></script>
####
pageDoYouWantToChangeEmail=
<div class="mainContainer" >
  <div class="body-container">
  </div>
</div>
<script>
  var fromTemplate = {
    body: "$_modalDoYouWantToChangeEmailBody()$",
    title: "$_modalDoYouWantToChangeEmailHeader()$"
  };
</script>
<script src="$cdnbaseurl$/compiled/all/change_email-$versioncode$.js" crossorigin="anonymous"></script>
####
