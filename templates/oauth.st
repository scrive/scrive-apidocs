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
  
  <div class="login" style="margin:12px;padding:12px;">
    <form id="loginForm" method="post" action="/oauth/authorizationconfirmlogin">
      <h2>Log in and confirm privileges</h2>
      <span class="txt">$_email()$</span><input type="email"
                                                name="email" value=""
                                                infotext="$_email()$"
                                                autocomplete="off" />
      <br />
      <span class="txt">$_password()$</span><input type="password"
                                                   name="password"
                                                   infotext="password"
                                                   autocomplete="off" />
      <br />
      <input type="hidden" value="$token$" name="oauth_token" />
      <input type="submit" class="hidden" />
      <div class="finalbuttonbox">
        <a class="submit green btn-small">
          <div class="left"></div>
          <div class="label">Log in and Confirm</div>
          <div class="right"></div>
        </a>
      </div>
    </form>
  </div>
  
  <div class="newuser" style="margin:12px;padding:12px;width:500px;">
    <h2>Create new account (3 free signatures per month!)</h2>
    <form method="post" action="/oauth/authorizationconfirmnewuser" class="requestAccount">
      <input class="shiny float-left"
             name="email" type="email"
             infotext="$_FillInEmail()$"
             autocomplete="off" />
      <input type="hidden" value="$token$" name="oauth_token" />
      <a href="#" class="submit new-btn-small green2 center float-right">
        <p>Create account</p>
      </a>
      <input type="submit" class="hidden" />
      <div class="clearfix" > </div>
    </form>
  </div>

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
###
####
apiDashboard =
<div class="tab-viewer apidashboard">
  <div class="tab-header">
  </div>
  <ul class="tabs">
    <li><a href="/account">$_HeadUserInfo()$</a></li>
    <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
    $if(iscompanyadmin)$<li><a href="/account/company">$_CompanySettings()$</a></li>$endif$
    $if(iscompanyadmin)$<li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
    <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
    <li><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
    <li class="active"><a href="/oauth/dashboard">API</a></li>
  </ul>
  <div class="tab-container">
    <div class="tab-content account">

      <h2 class="first">Granted Privileges</h2>

      $if(apiprivileges)$

      <p>
        You have granted the following companies or individuals
        privileges to act on your behalf.
      </p>


      <table>
        <thead>
          <tr>
            <td>Company/Individual</td>
            <td>Privilege</td>
            <td></td>
          </tr>
        </thead>
        <tbody>
          $apiprivileges:{apiprivilege|
          <tr>
            <td>
              $apiprivilege.name$
            </td>
            <td>
            </td>
            <td>
              <form method="POST" action="/oauth/deleteprivilege">
                <input type="hidden" name="tokenid"
                       value="$apiprivilege.tokenid$" />
                <a class="submit red btn-small">
                  <div class="left"></div>
                  <div class="label">Delete All</div>
                  <div class="right"></div>
                </a>
              </form>
            </td>
          </tr>
          $apiprivilege.privileges:{privilege|
          <tr>
            <td>
              $apiprivilege.name$
            </td>
            <td>
              $privilege$
            </td>
            <td>
              <form method="POST" action="/oauth/deleteprivilege">
                <input type="hidden" name="tokenid"
                       value="$apiprivilege.tokenid$" />
                <input type="hidden" name="privilege"
                       value="$apiprivilege.privilege$" />
                <a class="submit red btn-small">
                  <div class="left"></div>
                  <div class="label">Delete</div>
                  <div class="right"></div>
                </a>
              </form>
            </td>
          </tr>
          }$
          }$
        </tbody>
        <tfoot>
          <tr>
            <td></td>
            <td></td>
            <td></td>
          </tr>
        </tfoot>
      </table>
      $else$
      <p>You have granted no privileges to other companies or individuals.</p>
      $endif$

      <h2>Personal Access Token</h2>
      <p>
        Use this access token to access your own Scrive account
        through the API.
      </p>
      $if(personaltoken)$
      <table>
        <thead>
          <tr>
            <td>Access Token</td>
            <td>Access Secret</td>
            <td></td>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              $personaltoken.token$
            </td>
            <td>
              $personaltoken.secret$
            </td>
            <td>
              <form method="POST" action="/oauth/deletepersonaltoken">
                <a class="submit red btn-small">
                  <div class="left"></div>
                  <div class="label">Delete</div>
                  <div class="right"></div>
                </a>
              </form>
            </td>
          </tr>
        </tbody>
      </table>
      $else$
      <p>
        <form method="POST" action="/oauth/createpersonaltoken">
          <a class="submit green btn-small">
            <div class="left"></div>
            <div class="label">Create a personal token</div>
            <div class="right"></div>
          </a>
        </form>
      </p>
      $endif$



      <h2>API Tokens</h2>
      <p>
        API Tokens allow you to request access to other Scrive
        accounts using OAuth.
      </p>
      <table>
        <thead>
          <tr>
            <td>API Token</td>
            <td>API Secret</td>
            <td>
              <form method="POST" action="/oauth/createapitoken">
                <a class="submit green btn-small">
                  <div class="left"></div>
                  <div class="label">Create</div>
                  <div class="right"></div>
                </a>
              </form>
            </td>
          </tr>
        </thead>
        <tbody>
          $apitokens:{apitoken|
          <tr>
            <td>
              $apitoken.token$
            </td>
            <td>
              $apitoken.secret$
            </td>
            <td>
              <form method="POST" action="/oauth/deleteapitoken">
                <input type="hidden" name="apitoken"
                       value="$apitoken.token$" />
                <a class="submit red btn-small">
                  <div class="left"></div>
                  <div class="label">Delete</div>
                  <div class="right"></div>
                </a>
              </form>
            </td>
          </tr>
          }$
        </tbody>
        <tfoot>
          <tr>
            <td></td>
            <td></td>
            <td></td>
          </tr>
        </tfoot>
      </table>
    </div>
  </div>
</div>
####
