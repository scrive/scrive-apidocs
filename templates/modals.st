###
modalContractCreated=
<script>
  jQuery(document).ready(function() {
    var p = jQuery('</p>');
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
                        content: p});
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
modalRejectedView=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalRejectedViewHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalRejectedViewBody()$</p>
    </div>
  </div>
  <div class="modal-footer">
    <a class="btn-small green float-right close">
      <div class="left"></div>
      <div class="label">
        <p>$_close()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
</div>
#####
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
