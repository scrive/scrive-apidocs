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
statsOrgTotal=$_statsOrgTotal()$
####
pageDoYouWantToBeCompanyAccount=
<div class="mainContainer" >
  <div class="body-container">
  </div>
</div>
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<div/>').addClass('recovery-container');
    content.append(jQuery('<p/>').append('$_modalDoYouWantToBeCompanyAccountBody()$'));
    Confirmation.popup({onAccept: function() {
                                    new Submit({method: 'POST', url: ''}).send();
                                  },
                        onReject : function() { window.location = '/account' ; },
                        title: '$_modalDoYouWantToBeCompanyAccountHeader()$',
                        acceptButtonText: '$_modalDoYouWantToBeCompanyAccountFooterAccept()$',
                        content: content});
  });
</script>
####
pageDoYouWantToChangeEmail=
<div class="mainContainer" >
  <div class="body-container">
  </div>
</div>
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<div/>').addClass('recovery-container');
    content.append(jQuery('<p/>').append('$_modalDoYouWantToChangeEmailBody()$'));
    var password_input = jQuery('<input type="password" name="password" autocomplete="off"/>');
    content.append(jQuery('<p/>').append(password_input));
    Confirmation.popup({onAccept: function() {
                                    new Submit({method: 'POST',
                                                url: '',
                                                password: password_input.val()}).send();
                                  },
                        onReject : function() { window.location = '/account' ; },
                        title: '$_modalDoYouWantToChangeEmailHeader()$',
                        acceptButtonText: 'OK',
                        content: content});
  });
</script>
####