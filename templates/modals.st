###
modalContractCreated=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<div/>').addClass('body');
    var p = jQuery('</p>');
    content.append(p);
    var title;

    $if(signedAndCompleted)$
      p.append('$_modalContractCreatedBodyIfSignedAndCompleted()$');
      title = '$_modalContractCreatedHeaderIfSignedAndCompleted()$';
    $elseif(signedByAuthor)$
      p.append('$_modalContractCreatedBodyIfSignedAndSent()$');
      title = '$_modalContractCreatedHeaderIfSignedAndSent()$';
    $else$
      p.append('$_modalContractCreatedBodyIfSent()$');
      title = '$_modalContractCreatedHeaderIfSent()$';
    $endif$

    Confirmation.popup({title: title,
                        acceptButtonText: '$_close()$',
                        content: content});
  });
</script>
###
modalOfferCreated=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<p/>').append('$_modalOfferCreatedBody()$');
    Confirmation.popup({title: '$_modalOfferCreatedHeader()$',
                        acceptButtonText: '$_close()$',
                        content: content});
  });
</script>
###
modalOrderCreated=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<p/>').append('$_modalOrderCreatedBody()$');
    Confirmation.popup({title: '$_modalOrderCreatedHeader()$',
                        acceptButtonText: '$_close()$',
                        content: content});
  });
</script>
#####
modalDoYouWantToBeCompanyAccount=
<script>
  jQuery(document).ready(function() {
    var content = jQuery('<div/>').addClass('recovery-container');
    var body = jQuery('<div/>').addClass('body');
    content.append(body);
    body.append(jQuery('<p/>').append('$_modalDoYouWantToBeCompanyAccountBody()$'));
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
    var body = jQuery('<div/>').addClass('body');
    content.append(body);
    body.append(jQuery('<p/>').append('$_modalDoYouWantToChangeEmailBody()$'));
    var password_input = jQuery('<input type="password" name="password" autocomplete="off"/>');
    body.append(jQuery('<p/>').append(password_input));
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
