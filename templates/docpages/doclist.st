####
pageDocumentsList=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='archive'></div>
<script>
  jQuery(".archive").append(new Archive({
    month : $month$, 
    year : $year$, 
    forCompanyAdmin : $if(isadmin)$true$else$false$endif$,
    forNewUser: WelcomeUser.isFromPostSignView()
  }).view().el);

  if (WelcomeUser.isFromPostSignView()) {
    jQuery(".archive").append(WelcomeUser.fromPostSignViewWelcome());
  }

  mixpanel.register({Context : 'Archive'});
  mixpanel.track('View Archive');
</script>
</div>
</div>
####
