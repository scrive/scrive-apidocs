####
modalDoYouWantToBeCompanyAccount=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<div/>').addClass('recovery-container');
    content.append(jQuery('<p/>').append('$_modalDoYouWantToBeCompanyAccountBody()$'));
    Confirmation.popup({onAccept: function() {
                                    new Submit({method: 'POST', url: ''}).send();
                                  },
                        title: '$_modalDoYouWantToBeCompanyAccountHeader()$',
                        acceptButtonText: '$_modalDoYouWantToBeCompanyAccountFooterAccept()$',
                        content: content});
  });
</script>
####
modalDoYouWantToChangeEmail=
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
                        title: '$_modalDoYouWantToChangeEmailHeader()$',
                        acceptButtonText: 'OK',
                        content: content});
  });
</script>
####
modalDenyDelay=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<p/>').append('$_modalDenyDelay()$');
    Confirmation.popup({title: '$_signatoryMessageIfTimedout()$',
                        acceptButtonText: '$_close()$',
                        content: content});
  });
</script>
#####
modalConfirmDelay=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<p/>').append('$_modalConfirmDelay()$');
    Confirmation.popup({title: '$_modalUserSignupDoneHeader()$',
                        acceptButtonText: '$_close()$',
                        content: content});
  });
</script>
#####
