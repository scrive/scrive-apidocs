####
pagePrivilegesConfirm =
  $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
       <body>
        <div class='oauth' ></div>
        $pageFooter()$
        <script>
          jQuery(function() {
            var target = jQuery(".oauth");
            var confirmation =  new OAuthConfiration({ }).view().el
            target.append(confirmation);
          });
        </script>
      </body>
    </html>
#####
pagePrivilegesConfirmX = 
    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
       <body>
       $header()$
        <div class="mainContainer960" >
        <div class="body-container">
          <div class="oauth-confirm">
            <h2>$_importantSecurityInformation()$</h2>
            <div class='white box'>
              <p>
                $_isRequestingPermissions()$
              </p>
              $if(isDocumentCreate)$
              <h3>$_documentCreation()$</h3>
              <p>
                $_createDocumentsMessage()$
              </p>
              $endif$
              $if(isDocumentSend)$
              <h3>$_documentSend()$</h3>
              <p>
                $_sendDocumentsMessage()$
              </p>
              $endif$
              $if(isDocumentCheck)$
              <h3>$_documentCheck()$</h3>
              <p>
                $_checkDocumentsMessage()$
              </p>
              $endif$
              </div>
              <div class="buttons">
                <div class="floatright" style="margin-left: 34px">
                <form method="POST" action="/oauth/authorizationconfirm">
                  <div class="finalbuttonbox">
                    <a class="submit green btn-big">
                      <div class="left"></div>
                      <div class="label">$_confirm()$</div>
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
                    <a class="submit red btn-big">
                      <div class="left"></div>
                      <div class="label">$_deny()$</div>
                      <div class="right"></div>
                    </a>
                  </div>
                  <input type="hidden" name="oauth_token" value="$token$" />
                  <input type="submit" class="hidden" />
                </form>
                </div>
            </div>
          </div>
        </div>
        </div>
        $pageFooter()$
      </body>
    </html>


####
pagePrivilegesConfirmNoUser=
<div class="oauth-confirm">
     <div class="maintext">
  <h2>$_importantSecurityInformation()$</h2>
  <p>
    $_isRequestingPermissions()$
  </p>
  $if(isDocumentCreate)$
  <h3>$_documentCreation()$</h3>
  <p>
    $_createDocumentsMessage()$
  </p>
  $endif$
  </div>
  <div class="login option">
    <form id="loginForm" method="post" action="/oauth/authorizationconfirmlogin">
      <h2>$_logInAndConfirmPrivileges()$</h2>
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
          <div class="label">$_logInAndConfirm()$</div>
          <div class="right"></div>
        </a>
      </div>
    </form>
  </div>
  
  <div class="newuser option">
    <h2>$_createaccount()$</h2>
    <form method="post" action="/oauth/authorizationconfirmnewuser" class="requestAccount">
      <input class="shiny"
             name="email" type="email"
             infotext="$_FillInEmail()$"
             autocomplete="off" />
      <input type="hidden" value="$token$" name="oauth_token" />
      <a href="#" class="submit new-btn-small green2 center">
        <p>$_createaccount()$</p>
      </a>
      <input type="submit" class="hidden" />
      <div class="clearfix" > </div>
    </form>
  </div>
  <div class="deny option right">
    <h2>$_denyPrivileges()$</h2>
    <form method="POST" action="/oauth/authorizationdeny">
      <div class="finalbuttonbox">
        <a class="submit red btn-small">
          <div class="left"></div>
          <div class="label">$_deny()$</div>
          <div class="right"></div>
        </a>
      </div>
      <input type="hidden" name="oauth_token" value="$token$" />
      <input type="submit" class="hidden" />
    </form>
  </div>
</div>
###