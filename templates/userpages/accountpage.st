####
showAccount =
<div class="mainContainer" >
  <div class="body-container">
    <div class='blocking-box'></div>
    <div class='archive'></div>
    <script>
    var fromTemplate = {
      companyAdmin: $if(companyAdmin)$true$else$false$endif$,
      noCompany : $if(noCompany)$true$else$false$endif$
    };
    </script>
    <script src="/compiled/all/account.js"></script>
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
<script src="/compiled/all/company_account.js"></script>
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
<script src="/compiled/all/change_email.js"></script>
####
