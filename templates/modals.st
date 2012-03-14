###
modalSignInviteView=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    $if(signed)$
    <span class="modal-title">
      <h3>$_modalSignInviteViewHeaderIfSigned()$</h3>
    </span>
    $else$
    <span class="modal-title">
      <h3>$_modalSignInviteViewHeaderIfElse()$</h3>
    </span>
    $endif$
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $if(signed)$
      <p>
        $_modalSignInviteViewBodyIfSigned()$
      </p>
      $else$
      <p>
        $_modalSignInviteViewBodyIfElse()$
      </p>
      $endif$
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
modalOfferCreated=
<div class="modal-container jsnotfixed">
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
<div class="modal-container jsnotfixed">
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
modalOfferSignedHasAccount=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalOfferSignedHasAccountHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalOfferSignedHasAccountBody()$
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
modalOfferSignedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalOfferSignedNoAccountHeader()$</h3>
      <p>$_modalOfferSignedNoAccountBody()$</p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
    $accountFromSignFooter()$
  </div>
  </form>
</div>
$accountFromSignScript()$
######
modalAccountSetup=
<div class="modal-container">
  <script src="/js/signup.js" type="text/javascript"></script>
  <form action="$signuplink$" method="post">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalAccountSetupHeaderAcceptTOS()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body" style="padding: 10px">
    <div class="modal-content">
      <div style="max-height: 300px; overflow: auto">
        $termsPageContents()$
      </div>
      <div style="padding: 0 19px;">
        <div style="padding-bottom:10px;">
          <input type="checkbox" id="tosCBox" name="tos" />
          <label for="tosCBox">$_modalAccountSetupBodyAcceptTOS()$</label><br/>
        </div>
        <table id="signupInfo"><tbody>
            <tr>
                <td>$_firstname()$:</td>
                <td><input type="text" name="fstname" value="$fstname$" autocomplete="off" /></td>
            </tr>
            <tr>
                <td>$_lastname()$:</td>
                <td><input type="text" name="sndname" value="$sndname$" autocomplete="off" /></td>
            </tr>
            <tr>
                <td style="width: 120px">$_modalAccountSetupChoosePassword()$</td>
                <td><input type="password" name="password" autocomplete="off"/></td>
            </tr>
            <tr>
                <td>$_modalAccountSetupRepeatPassword()$</td>
                <td><input type="password" name="password2" autocomplete="off"/></td>
            </tr>
            <tr style='height:20px;'>
                <td colspan="2"><input type='checkbox' style='padding: 0px;margin:0px 5px 0px 0px;float:left;' class="callme" id="callme" name='callme'/>
                <label for="callme">$_modalAccountSetupCallMe()$</label></td>
                <td></td>
            </tr>
            <tr class='phonerow' style="display:none;">
                <td>$_modalAccountSetupPhone()$</td>
                <td><input type="text" name="phone" autocomplete="off"/></td>
            </tr>
        </tbody></table>
        <input type="submit" class="hidden" />
      </div>
    </div>
  </div>
  <div class="modal-footer">
    <a href="#" class="btn-small green float-right submit">
      <div class="left"></div>
      <div class="label">
        <p>$_modalAccountSetupFooter()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
  </form>
</div>
<script>
window.onload = function() {
jQuery('form[action="$signuplink$"]').submit(function(){
    var callback = function(t,e,v) {
        e.css("background", "red"); 
        jQuery(document.createElement('div'))
            .attr("name", "validate-message")
            .addClass('failed-validation')
            .css({"font-size": 8, "font-weight": "bold", "color": "red"})
            .append(v.message())
            .appendTo(e.parent());
    };
    var fstname   = jQuery('#signupInfo').find('input[name="fstname"]');
    var sndname   = jQuery('#signupInfo').find('input[name="sndname"]');
    var password  = jQuery('#signupInfo').find('input[name="password"]');
    var password2 = jQuery('#signupInfo').find('input[name="password2"]');
    var callme    = jQuery('#signupInfo').find('input[name="callme"]');
    var phone     = jQuery('#signupInfo').find('input[name="phone"]');
    var tos       = jQuery('#tosCBox');

    fstname.css("background", "white");
    sndname.css("background", "white");
    password.css("background", "white");
    password2.css("background", "white");
    tos.css("background", "white");

    jQuery('[name=validate-message]').remove();

    var vresult = [
        fstname.validate(new NameValidation({callback: callback, message: localization.validation.firstNameRequired})),
        sndname.validate(new NameValidation({callback: callback, message: localization.validation.secondNameRequired})),
        password.validate(new PasswordValidation({
              callback: callback
            , message: localization.validation.passwordLessThanMinLength
            , message_max: localization.validation.passwordExceedsMaxLength
            , message_digits: localization.validation.passwordNeedsLetterAndDigit
            })),
        password2.validate(new PasswordEqValidation({
              callback: callback
            , message: localization.validation.passwordsDontMatch
            , "with": password
            })),
        tos.validate(new CheckboxReqValidation({callback: callback, message: localization.validation.mustAcceptTOS}))
        ];

    if (callme.attr('checked'))
        vresult.push(phone.validate(new NotEmptyValidation({callback: callback, message: localization.validation.phoneRequired})));

    var ret = true;

    jQuery.each(vresult, function(k,v) { if(!v) ret = false; });

    return ret;
});
}
</script>
###
modalSendInviteView=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSendInviteViewHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalSendInviteViewBody()$
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
######
######
modalLoginForSaveView=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalLoginForSaveViewHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalLoginForSaveViewBody()$</p>
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
modalSignedViewClosedHasAccount=
$if(isloggedin)$
$modalSignedViewClosedHasAccountLoggedIn()$
$else$
$modalSignedViewClosedHasAccountNotLoggedIn()$
$endif$
####
modalSignedViewClosedHasAccountNotLoggedIn=
<div class="modal-container">
  <form method="post" action="$linklogin$">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>$_modalSignedViewClosedHasAccountNotLoggedInHeader()$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>$_modalSignedViewClosedHasAccountNotLoggedInBodySigned()$
</p>
        <p>$_modalSignedViewClosedHasAccountNotLoggedInBodySaved()$
</p>
        $loginModalPart()$
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">$_close()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewClosedHasAccountNotLoggedInFooterForgottenPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_login()$</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </form>
</div>
####
modalSignedViewClosedHasAccountLoggedIn=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewClosedHasAccountLoggedInHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalSignedViewClosedHasAccountLoggedInBodySigned()$</p>
      <p>$_modalSignedViewClosedHasAccountLoggedInBodySaved()$</p>
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
####
modalSignedViewOrderClosedHasAccount=
$if(isloggedin)$
$modalSignedViewOrderClosedHasAccountLoggedIn()$
$else$
$modalSignedViewOrderClosedHasAccountNotLoggedIn()$
$endif$
####
modalSignedViewOrderClosedHasAccountNotLoggedIn=
<div class="modal-container">
  <form method="post" action="$linklogin$">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>$_modalSignedViewOrderClosedHasAccountNotLoggedInHeader()$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>$_modalSignedViewOrderClosedHasAccountNotLoggedInBodyConfirmed()$</p>
        <p>$_modalSignedViewOrderClosedHasAccountNotLoggedInBodySaved()$</p>
        $loginModalPart()$
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">$_close()$</a>
        <a href="#" class="txt-link message">$_forgotpassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_login()$</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </form>
</div>
####
modalSignedViewOrderClosedHasAccountLoggedIn=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewOrderClosedHasAccountLoggedInHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalSignedViewOrderClosedHasAccountLoggedInBodyConfirmed()$</p>
      <p>$_modalSignedViewOrderClosedHasAccountLoggedInBodySaved()$</p>
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
modalSignedViewClosedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewClosedNoAccountHeader()$</h3>
      <p>$_modalSignedViewClosedNoAccountBody()$</p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
      $accountFromSignFooter()$
  </div>
  </form>
</div>
$accountFromSignScript()$
####
####
modalSignedViewOrderClosedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewOrderClosedNoAccountHeader()$</h3>
      <p>$_modalSignedViewOrderClosedNoAccountBody()$</p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
      $accountFromSignFooter()$
  </div>
  </form>
</div>
$accountFromSignScript()$
####
modalSignedViewNotClosedHasAccount=
$if(isloggedin)$
$modalSignedViewNotClosedHasAccountLoggedIn()$
$else$
$modalSignedViewNotClosedHasAccountNotLoggedIn()$
$endif$
#####
modalSignedViewNotClosedHasAccountLoggedIn=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewNotClosedHasAccountLoggedInHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalSignedViewNotClosedHasAccountLoggedInBodyNotSigned()$</p>
      <p>$_modalSignedViewNotClosedHasAccountLoggedInBodySaved()$</p>
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
#####
modalSignedViewNotClosedHasAccountNotLoggedIn=
<div class="modal-container">
  <form method="post" action="$linklogin$">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>$_modalSignedViewNotClosedHasAccountNotLoggedInHeader()$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>$_modalSignedViewNotClosedHasAccountNotLoggedInBodyNotSigned()$</p>
        <p>$_modalSignedViewNotClosedHasAccountNotLoggedInBodySaved()$</p>
        $loginModalPart()$
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">$_close()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewNotClosedHasAccountNotLoggedInFooterForgottenPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_login()$</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </form>
</div>
#####
modalSignedViewNotClosedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewNotClosedNoAccountHeader()$</h3>
      <p>
        $_modalSignedViewNotClosedNoAccountBody()$
      </p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
    $accountFromSignFooter()$
  </div>
  </form>
</div>
$accountFromSignScript()$
#####
modalSignedViewOrderNotClosedHasAccount=
$if(isloggedin)$
$modalSignedViewOrderNotClosedHasAccountLoggedIn()$
$else$
$modalSignedViewOrderNotClosedHasAccountNotLoggedIn()$
$endif$
#####
modalSignedViewOrderNotClosedHasAccountLoggedIn=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewOrderNotClosedHasAccountLoggedInHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalSignedViewOrderNotClosedHasAccountLoggedInBodyNotSigned()$</p>
      <p>$_modalSignedViewOrderNotClosedHasAccountLoggedInBodySaved()$</p>
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
#####
modalSignedViewOrderNotClosedHasAccountNotLoggedIn=
<div class="modal-container">
  <form method="post" action="$linklogin$">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>$_modalSignedViewOrderNotClosedHasAccountNotLoggedInHeader()$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>$_modalSignedViewOrderNotClosedHasAccountNotLoggedInBodyNotConfirmed()$</p>
        <p>$_modalSignedViewOrderNotClosedHasAccountNotLoggedInBodySaved()$</p>
        $loginModalPart()$
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">$_close()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewOrderNotClosedHasAccountNotLoggedInFooterForgottenPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_login()$</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </form>
</div>
#####
modalSignedViewOrderNotClosedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalSignedViewOrderNotClosedNoAccountHeader()$</h3>
      <p>$_modalSignedViewOrderNotClosedNoAccountBody()$</p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
    $accountFromSignFooter()$
  </div>
  </form>
</div>
$accountFromSignScript()$
#####
accountFromSignScript=
<script>
window.onload = function() {
jQuery('#accountFromSignForm').submit(function(){
    var callback = function(t,e,v) {
        e.css("background", "red"); 
        jQuery(document.createElement('div'))
            .attr("name", "validate-message")
            .addClass('failed-validation')
            .css({"font-size": 8, "font-weight": "bold", "color": "red"})
            .append(v.message())
            .appendTo(e.parent());
    };

    var password  = jQuery('.account-from-sign-container').find('input[name="password"]');
    var password2 = jQuery('.account-from-sign-container').find('input[name="password2"]');
    var phone     = jQuery('.account-from-sign-container').find('input[name="phone"]');
    var callme    = jQuery('.account-from-sign-container').find('input[name="callme"]');
    var tos       = jQuery('#tosCBox');

    password.css("background", "white");
    password2.css("background", "white");
    tos.css("background", "white");

    jQuery('[name=validate-message]').remove();

    var vresult = [
        password.validate(new PasswordValidation({
              callback: callback 
            , message: localization.validation.passwordLessThanMinLength
            , message_max: localization.validation.passwordExceedsMaxLength
            , message_digits: localization.validation.passwordNeedsLetterAndDigit
            })),
        password2.validate(new PasswordEqValidation({
              callback: callback
            , message: localization.validation.passwordsDontMatch
            , "with": password
            })),
        tos.validate(new CheckboxReqValidation({callback: callback, message: localization.validation.mustAcceptTOS}))
        ];

    if (callme.attr('checked'))
        vresult.push(phone.validate(new NotEmptyValidation({callback: callback, message: localization.validation.phoneRequired})));

    var ret = true;

    jQuery.each(vresult, function(k,v) { if(!v) ret = false; });

    return ret;
});
}
</script>
#####
accountFromSignInputs=
<div class="account-from-sign-container">
  <div class="login-container">
    <div class="body">
      <div class="wrapper">
        <p>$_accountFromSignInputsBodySecureOriginal()$</p>
        <input type="hidden" name="acceptaccount" value="true" />
        <div style="padding:10px 50px;">
         <div>
          <input type="checkbox" name="tos" id="tosCBox" style='padding: 0px;margin:0px 5px 0px 0px;float:left;width: auto;'/>
          <label for="tos" >$_accountFromSignInputsBodyAcceptTOS()$</label>
         </div>
         <div style="padding-top:8px">
             <input type='checkbox' style='padding: 0px;margin:0px 5px 0px 0px;float:left;width: auto;' class="callme" name='callme'/>$_modalAccountSetupCallMe()$
             <script src="/js/signup.js" type="text/javascript"></script>
         </div>
         <div style="padding-top:10px">
          <table>
            <tr>
              <td style="width:120px;">$_password()$</td>
              <td><input type="password" name="password" /></td>
            </tr>
            <tr>
              <td>$_accountFromSignInputsConfirmPassword()$</td>
              <td><input type="password" name="password2" /></td>
            </tr>
            <tr class='phonerow' style="display:none;">
                <td>$_modalAccountSetupPhone()$</td>
                <td><input type="text" name="phone" autocomplete="off"/></td>
            </tr>
          </table>
          <input type="submit" class="hidden" />
         </div>
        </div>
      </div>
    </div>
  </div>
</div>

#####
accountFromSignFooter=
    <div class="accountFromSignFooter">
      <div class="float-left">
        <a class="cancel close">$_accountFromSignFooterNoThx()$</a>
      </div>
      <a class="btn-small green float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_accountFromSignFooterYesArchive()$</p>
          <div class="btn-symbol green arrow-left"></div>
        </div>
        <div class="right"></div>
      </a>
    </div>
#####
#####
loginModal=
$if(!logged)$
$if(loginModal)$
<div class="modalcontainer">
  <div class="modalbox overlay login-container">
    <div class="modal-container">
      <form id="loginForm" method="post" action="$linklogin$">
        <div class="modal-header">
          <span class="modal-icon message"></span>
          <span class="modal-title">
            <h1>$_login()$</h1>
          </span>
          <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
          <div class="modal-content">
            <div class="body">
              <div class="wrapper" style="text-align:right">
                <input type="hidden" name="referer" value="$referer$" />
                <span class="txt">$_email()$</span><input type="email" name="email" value="$email$" infotext="$_email()$" autocomplete="off"/><br />
                <span class="txt">$_password()$</span><input type="password" name="password" infotext="password" autocomplete="off"/><br />
                <input type="submit" class="hidden" />
              </div>
            </div>
          </div>
        </div>
        <div class="modal-footer">
          <div class="float-left">
            <a class="close cancel">$_cancel()$</a>
            <a href="#" class="txt-link message">$_forgotpassword()$</a>
          </div>
          <a href="#" class="btn-small blue float-right submit">
            <div class="left"></div>
            <div class="label">
              <p>$_login()$</p>
            </div>
            <div class="right"></div>
          </a>
        </div>
      </form>
    </div>
  </div>

  <div class="modalbox overlay recovery-container">
    <div class="modal-container">
      <form method="post" action="$linkforgotenpassword$">
        <div class="modal-header">
          <span class="modal-icon message"></span>
          <span class="modal-title">
            <h1>$_forgotpassword()$</h1>
          </span>
          <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
          <div class="modal-content">
            <div class="body">
              <div class="wrapper">
                <span class="txt">$_email()$</span><input type="email" name="email" infotext="$_email()$" autocomplete="off"/>
                <input type="submit" class="hidden" />
              </div>
            </div>
          </div>
        </div>
        <div class="modal-footer">
          <div class="float-left">
            <a class="close cancel">$_cancel()$</a>
            <a href="#" class="txt-link message">$_login()$</a>
          </div>
          <a href="#" class="btn-small green float-right submit">
            <div class="left"></div>
            <div class="label">
              <p>$_loginModalForgotPasswordSendNewPassword()$</p>
            </div>
            <div class="right"></div>
          </a>
        </div>
      </form>
    </div>
  </div>
</div>
$endif$
$endif$
####
loginModalPart=
<div class="user-container">
  <div class="login-container">
    <div class="body">
      <div class="wrapper" style="text-align:right">
        <input type="hidden" name="referer" value="$referer$" />
        <span class="txt">$_email()$</span><input type="email" name="email" value="$email$" infotext="$_email()$" autocomplete="off"/><br />
        <span class="txt">$_password()$</span><input type="password" name="password" infotext="password" autocomplete="off"/><br />
        <input type="submit" class="hidden" />
      </div>
    </div>
  </div>
</div>
######
####
modalNewPasswordView=
<form action="$linkchangepassword$" method="post">
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h1>$_modalNewPasswordViewHeader()$</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <div class="recovery-container">
          <div class="body">
            <div class="wrapper" style="text-align: right;">
              <span class="txt">$_modalNewPasswordViewNewPassword()$</span>
              <input type="password" name="password" autocomplete="off" />
              <BR/>
              <span class="txt">$_modalNewPasswordViewRepeatPassword()$</span>
              <input type="password" name="password2" autocomplete="off" />
              <BR/>
              <input type="submit" class="hidden"/>
            </div>
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
          <p>$_modalNewPasswordViewFooterSave()$</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </div>
</form>
####
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
####
modalDoYouWantToChangeEmail=
<form action="" method="post">
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
            <h1>$_modalDoYouWantToChangeEmailHeader()$</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
            <div class="recovery-container">
              <div class="body">
                <p>$_modalDoYouWantToChangeEmailBody()$</p>
                <p>
                  <input type="password" name="password" autocomplete="off"/>
                </p>
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
              <p>$_modalDoYouWantToChangeEmailAccept()$</p>
            </div>
            <div class="right"></div>
      </a>
    </div>
  </div>
</form>
####
###
modalError=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalErrorHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalErrorBody()$</p>
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
signAwaitingAuthorLast=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_signAwaitingAuthorLastHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_signAwaitingAuthorLastBody()$</p>
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
pdfTooBigModal=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h1>$_modalPdfTooBigTitle()$</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <div class="recovery-container">
          <div class="body">
            $_modalPdfTooBigBody()$
          </div>
        </div>
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">$_cancel()$</a>
      </div>
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
####
modalUserSignupDone=
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
      <p>$_modalUserSignupDoneBody()$</p>
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
