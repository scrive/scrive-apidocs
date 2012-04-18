####
pagePrivilegesConfirm=
<div class="oauth-confirm">
  <h2>Important Security Information</h2>
  <p>
    <em>$companyname$</em> is requesting permission to perform the
    following operations on your behalf (the Scrive account for email
    $email$). Please review them carefully and either confirm or deny
    them.
  </p>
  $if(isDocumentCreate)$
  <h3>Document Creation</h3>
  <p>
    If confirmed, <em>$companyname$</em> will be able to create
    documents for you. These documents would not be sent to anyone.
    They will show up in your archive.
  </p>
  $endif$

  <div class="buttons">
    <div class="floatright" style="margin-left: 34px">
    <form method="POST" action="/oauth/authorizationconfirm">
      <div class="finalbuttonbox">
        <a class="submit green btn-small">
          <div class="left"></div>
          <div class="label">Confirm</div>
          <div class="right"></div>
        </a>
      </div>
      <input type="hidden" name="oauth_token" value="$token$" />
      <input type="submit" class="hidden" />
    </form>
    </div>
    <div class="floatright">
    <form method="POST" action="/oauth/authorizationdeny">
      <div class="finalbuttonbox">
        <a class="submit red btn-small">
          <div class="left"></div>
          <div class="label">Deny</div>
          <div class="right"></div>
        </a>
      </div>
      <input type="hidden" name="oauth_token" value="$token$" />
      <input type="submit" class="hidden" />
    </form>
    </div>
  </div>
</div>
###
pagePrivilegesConfirmNoUser=
<div class="oauth-confirm">
  <h2>Important Security Information</h2>
  <p>
    <em>$companyname$</em> is requesting permission to perform the
    following operations on your behalf. Please review them carefully
    and either confirm or deny them.
  </p>
  $if(isDocumentCreate)$
  <h3>Document Creation</h3>
  <p>
    If confirmed, <em>$companyname$</em> will be able to create
    documents for you. These documents would not be sent to anyone.
    They will show up in your archive.
  </p>
  $endif$

  <div class="login">
    <form id="loginForm" method="post" action="/oauth/authorizationconfirmlogin">
      <h2>Log in and confirm privileges</h2>
      <span class="txt">$_email()$</span><input type="email" name="email" value="" infotext="$_email()$" autocomplete="off"/><br />
      <span class="txt">$_password()$</span><input type="password"
      name="password" infotext="password" autocomplete="off"/><br />
      <input type="hidden" value="$token$" name="oauth_token" />
      <input type="submit" class="hidden" />
      <div class="finalbuttonbox">
        <a class="submit green btn-small">
          <div class="left"></div>
          <div class="label">Confirm</div>
          <div class="right"></div>
        </a>
      </div>
    </form>
  </div>
  
  <div class="newuser">
    <h2>Create new account (3 free signatures per month!)</h2>
    <form method="post" action="/oauth/authorizationconfirmnewuser" class="requestAccount">
      <input class="shiny float-left"
             name="email" type="email"
             infotext="$_FillInEmail()$"
             autocomplete="off"
             />
      <input type="hidden" value="$token$" name="oauth_token" />
      <a href="#" class="submit
                         new-btn-small green2 center
                         float-right"><p>$_ButtonCreateAccount()$!</p></a>
      <input type="submit" class="hidden" />
      <div class="clearfix" />
    </form>
  </div>
</div>
###
