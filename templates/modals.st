###
modalContractCreated=
  <div class="modal-header">
    <span class="modal-icon message"></span>
    $if(signedAndCompleted)$
    <span class="modal-title">
      $_modalContractCreatedHeaderIfSignedAndCompleted()$
    </span>
    $elseif(signedByAuthor)$
    <span class="modal-title">
      $_modalContractCreatedHeaderIfSignedAndSent()$
    </span>
    $else$
    <span class="modal-title">
      $_modalContractCreatedHeaderIfSent()$
    </span>
    $endif$
    <a class="modal-close close" onclick="closeAllActiveModals()"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <div class="body">
      $if(signedAndCompleted)$
      <p>
        $_modalContractCreatedBodyIfSignedAndCompleted()$
      </p>
      $elseif(signedByAuthor)$
      <p>
        $_modalContractCreatedBodyIfSignedAndSent()$
      </p>
      $else$
      <p>
        $_modalContractCreatedBodyIfSent()$
      </p>
      $endif$
      </div>
    </div>
  </div>
  <div class="modal-footer">
      <a class="float-right green button button-small button-green" onclick="closeAllActiveModals()"><div class="label">$_close()$</div></a>
  </div>
###
modalOfferCreated=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalOfferCreatedHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalOfferCreatedBody()$
      </p>
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
###
modalOrderCreated=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalOrderCreatedHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalOrderCreatedBody()$
      </p>
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
####
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
