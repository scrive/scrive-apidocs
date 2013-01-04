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
<form action="" method="post">
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
	    <h1>$_modalDoYouWantToBeCompanyAccountHeader()$</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
	    <div class="recovery-container">
	      <div class="body">
	        <p>$_modalDoYouWantToBeCompanyAccountBody()$</p>
	      </div>
	    </div>
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
	    <a class="close cancel">$_cancel()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
	    <div class="left"></div>
	    <div class="label">
	      <p>$_modalDoYouWantToBeCompanyAccountFooterAccept()$</p>
	    </div>
	    <div class="right"></div>
      </a>
    </div>
  </div>
</form>
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
signCanceledDataMismatchModal=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_signCanceledDataMismatchModalHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_signCanceledDataMismatchModalBodySorry()$</p>
      <p>$_signCanceledDataMismatchModalBodyAborted()$</p>
      <p><strong>$_signCanceledDataMismatchModalErrorReport()$</strong></p>
      $message:noescape()$
    </div>
  </div>
  <div class="modal-footer">
    <a class="btn-small green float-right close">
      <div class="left"></div>
      <div class="label">
        <p>$_ok()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
</div>
###
modalDenyDelay=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_signatoryMessageIfTimedout()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalDenyDelay()$</p>
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
modalConfirmDelay=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalUserSignupDoneHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalConfirmDelay()$</p>
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
