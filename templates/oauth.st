####
pagePrivilegesConfirm=
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
