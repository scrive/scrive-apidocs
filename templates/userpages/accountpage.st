####
showAccount =
<div class="mainContainer" >
  <div class="body-container">
    <div class='blocking-box'></div>
    <div class='archive'></div>
    <script>
      var account = new Account({companyAdmin : $if(companyAdmin)$true$else$false$endif$, noCompany : $if(noCompany)$true$else$false$endif$});
      jQuery(".body-container").append(account.el());
      mixpanel.register({Context : 'Account Page'});
      mixpanel.track('View Account Page');
    </script>
  </div>
</div>

#####
