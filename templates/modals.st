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
        <p>$_modalSignInviteViewFooterClose()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
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
        <p>$_modalOfferCreatedClose()$</p>
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
        <p>$_modalOrderCreatedClose()$</p>
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
        <p>$_modalOfferSignedHasAccountClose()$</p>
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
      <div style="padding: 0 19px">
        <input type="checkbox" id="tosCBox" name="tos" />
        <label for="tosCBox">$_modalAccountSetupBodyAcceptTOS()$</label><br/>
        <table id="accountTypeChoice" style="width: 250px; margin-bottom: 5px"><tbody><tr>
            <td><input id="accountTypePrivate" type="radio" value="PrivateAccount" $if(hassupervisor)$disabled="true"$else$name="accounttype"$endif$ $if(PrivateAccount)$checked="true"$endif$ /> <label for="accountTypePrivate">$_modalAccountSetupIfPrivateAccount()$</label></td>
            <td><input id="accountTypeCompany" type="radio" value="CompanyAccount" $if(hassupervisor)$disabled="true"$else$name="accounttype"$endif$ $if(CompanyAccount)$checked="true"$endif$ /> <label for="accountTypeCompany">$_modalAccountSetupIfCompanyAccount()$</label></td>
            $if(hassupervisor)$
            <td><input type="hidden" name="accounttype" value="$supervisoraccounttype$" /></td>
            $endif$
        </tr></tbody></table>
        <table id="companyInfo"><tbody>
            <tr>
                <td style="width: 120px">$_modalAccountSetupFirstName()$</td>
                <td><input type="text" name="fname" autocomplete="off" value="$if(fstname)$$fstname$$endif$"/></td>
            </tr>
            <tr>
                <td>$_modalAccountSetupLastName()$</td>
                <td><input type="text" name="lname" autocomplete="off" value="$if(sndname)$$sndname$$endif$"/></td>
            </tr>
            <tr>
                <td>$_modalAccountSetupCompany()$</td>
                <td><input type="text" name="companyname" autocomplete="off"
                    $if(hassupervisor)$
                    disabled="true" value="$supervisorcompany$"
                    $else$
                    value="$if(companyname)$$companyname$$endif$"
                    $endif$
                /></td>
            </tr>
            <tr>
                <td>$_modalAccountSetupPosition()$</td>
                <td><input type="text" name="companyposition" autocomplete="off" value="$if(companyposition)$$companyposition$$endif$"/></td>
            </tr>
            <tr>
                <td>$_modalAccountSetupPhone()$</td>
                <td><input type="text" name="phone" autocomplete="off" value="$if(phone)$$phone$$endif$"/></td>
            </tr>
        </tbody></table>
        <table id="passwordInfo"><tbody>
            <tr>
                <td style="width: 120px">$_modalAccountSetupChoosePassword()$</td>
                <td><input type="password" name="password" autocomplete="off"/></td>
            </tr>
            <tr>
                <td>$_modalAccountSetupRepeatPassword()$</td>
                <td><input type="password" name="password2" autocomplete="off"/></td>
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
######
modalWelcomeToSkrivaPa=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalWelcomeToSkrivaPaHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalWelcomeToSkrivaPaBodyMain()$</p>

      <p>$_modalWelcomeToSkrivaPaBodyInstructions()$</p>

      <p>$_modalWelcomeToSkrivaPaBodyHint()$</p>

      <p>$_modalWelcomeToSkrivaPaBodyBestRegards()$<br/>
        Team SkrivaPå</p>
    </div>
  </div>
  <div class="modal-footer">
    <a class="btn-small green float-right close">
      <div class="left"></div>
      <div class="label">
        <p>$_modalWelcomeToSkrivaPaFooter()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
</div>
####
modalAccountRemoval=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalAccountRemovalHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalAccountRemovalBody()$</p>
    </div>
  </div>
  <div class="modal-footer">
    <form method="post" action="$removallink$" id="removeaccountform"></form>
    <a href="#" onclick="\$('#removeaccountform').submit();" class="btn-small red float-right">
      <div class="left"></div>
      <div class="label">
        <p>$_modalAccountRemovalFooter()$</p>
        <div class="btn-symbol red arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
    <a href="$activationlink$" class="float-left" style="color: #5eb30c; text-decoration: none">$_modalAccountRemovalActivateAccount()$</a>
  </div>
</div>
###
modalAccountRemoved=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>$_modalAccountRemovedHeader()$</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      <p>$_modalAccountRemovedBody()$</p>
      <p>$_modalAccountRemovedBestRegards()$</p>
    </div>
  </div>
  <div class="modal-footer">
    <a class="btn-small green float-right close">
      <div class="left"></div>
      <div class="label">
        <p>$_modalAccountRemovedFooter()$</p>
        <div class="btn-symbol green arrow-left"></div>
      </div>
      <div class="right"></div>
    </a>
  </div>
</div>
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
        <p>$_modalSendInviteViewClose()$</p>
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
        <p>$_modalLoginForSaveViewClose()$</p>
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
        <p>$_modalRejectedViewClose()$</p>
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
        <a class="close cancel">$_modalSignedViewClosedHasAccountNotLoggedInFooterClose()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewClosedHasAccountNotLoggedInFooterForgottenPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_modalSignedViewClosedHasAccountNotLoggedInFooterLogIn()$</p>
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
        <p>$_modalSignedViewClosedHasAccountLoggedInClose()$</p>
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
        <a class="close cancel">$_modalSignedViewOrderClosedHasAccountNotLoggedInFooterClose()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewOrderClosedHasAccountNotLoggedInFooterForgotPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_modalSignedViewOrderClosedHasAccountNotLoggedInFooterLogIn()$</p>
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
        <p>$_modalSignedViewOrderClosedHasAccountLoggedInFooterClose()$</p>
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
        <p>$_modalSignedViewNotClosedHasAccountLoggedInFooterClose()$</p>
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
        <a class="close cancel">$_modalSignedViewNotClosedHasAccountNotLoggedInFooterCLose()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewNotClosedHasAccountNotLoggedInFooterForgottenPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_modalSignedViewNotClosedHasAccountNotLoggedInFooterLogIn()$</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </form>
</div>
#####
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
        <p>$_modalSignedViewOrderNotClosedHasAccountLoggedInFooterClose()$</p>
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
        <a class="close cancel">$_modalSignedViewOrderNotClosedHasAccountNotLoggedInFooterClose()$</a>
        <a href="#" class="txt-link message">$_modalSignedViewOrderNotClosedHasAccountNotLoggedInFooterForgottenPassword()$</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>$_modalSignedViewOrderNotClosedHasAccountNotLoggedInFooterLogIn()$</p>
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
          <input type="checkbox" name="tos" id="tos" style="width: auto"/>
          <label for="tos" style="padding-left:5px;">$_accountFromSignInputsBodyAcceptTOS()$</label>
         </div>
         <table style="width: 250px; margin-top: 5px"><tbody><tr>
           <td><input style="width: auto" id="accountTypePrivate" type="radio" name="accounttype" value="PrivateAccount" /> <label for="accountTypePrivate">$_accountFromSignInputsPrivateAccount()$</label></td>
           <td><input style="width: auto" id="accountTypeCompany" type="radio" name="accounttype" value="CompanyAccount" /> <label for="accountTypeCompany">$_accountFromSignInputsCompanyAccount()$</label></td>
         </tr></tbody></table>
         <div style="padding-top:10px">
          <table>
            <tr>
              <td style="width:120px;">$_accountFromSignInputsPassword()$</td>
              <td><input type="password" name="password" /></td>
            </tr>
            <tr>
              <td>$_accountFromSignInputsConfirmPassword()$</td>
              <td><input type="password" name="password2" /></td>
            </tr>
          </table>
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
        <a class="declineAccountFromSign cancel" style="width:60px;">$_accountFromSignFooterNoThx()$</a>
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
            <h1>$_loginModalLogIn()$</h1>
          </span>
          <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
          <div class="modal-content"> 
            <div class="body">
              <div class="wrapper" style="text-align:right">
                <input type="hidden" name="referer" value="$referer$" />
                <span class="txt">$_loginModalEmail()$</span><input type="email" name="email" value="$email$" infotext="E-post" autocomplete="off"/><br />
                <span class="txt">$_loginModalPassword()$</span><input type="password" name="password" infotext="password" autocomplete="off"/><br />
                <input type="submit" class="hidden" />
              </div>
            </div>
          </div>
        </div>
        <div class="modal-footer">
          <div class="float-left">
            <a class="close cancel">$_cancel()$</a>
            <a href="#" class="txt-link message">$_loginModalFooterForgotPassword()$</a>
          </div>
          <a href="#" class="btn-small blue float-right submit">
            <div class="left"></div>
            <div class="label">
              <p>$_loginModalFooterLogIn()$</p>
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
            <h1>$_loginModalForgotPasswordForgotPassword()$</h1>
          </span>
          <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
          <div class="modal-content"> 
            <div class="body">
              <div class="wrapper">
                <span class="txt">$_loginModalForgotPasswordEmail()$</span><input type="email" name="email" infotext="E-post" autocomplete="off"/>
                <input type="submit" class="hidden" />
              </div>
            </div>
          </div>
        </div>  
        <div class="modal-footer">
          <div class="float-left">
            <a class="close cancel">$_cancel()$</a>
            <a href="#" class="txt-link message">$_loginModalForgotPasswordFooterLogIn()$</a>
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
        <span class="txt">$_loginModalPartEmail()$</span><input type="email" name="email" value="$email$" infotext="E-post" autocomplete="off"/><br />
        <span class="txt">$_loginModalPartPassword()$</span><input type="password" name="password" infotext="password" autocomplete="off"/><br />
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
modalInviteUserAsSubaccount=
<form action="" method="post">
  <input type="hidden" name="takeover" value="really" />
  <input type="hidden" name="email" value="$email$" />
  <input type="hidden" name="fstname" value="$fstname$" />
  <input type="hidden" name="sndname" value="$sndname$" />
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
	    <h1>$_modalInviteUserAsSubaccountHeaderSendRequest()$</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">   
      <div class="modal-content"> 
	    <div class="recovery-container">
	      <div class="body">
	        <p>$_modalInviteUserAsSubaccountBodySendRequest()$</p>
	        <p>$_modalInviteUserAsSubaccountBodySendSure()$</p>
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
	      <p>$_modalInviteUserAsSubaccountFooterSendRequest()$</p>
	    </div>
	    <div class="right"></div>
      </a>
    </div>
  </div>
</form>
####
####
modalDoYouWantToBeSubaccount=
<form action="" method="post">
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
	    <h1>$_modalDoYouWantToBeSubaccountHeader()$</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">   
      <div class="modal-content"> 
	    <div class="recovery-container">
	      <div class="body">
	        <p>$_modalDoYouWantToBeSubaccountBody()$</p>
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
	      <p>$_modalDoYouWantToBeSubaccountFooterAccept()$</p>
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

