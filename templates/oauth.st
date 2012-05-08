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
  
  <div class="login option">
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
  
  <div class="newuser option">
    <h2>Create new account 
      <br /> (3 free signatures per month!)</h2>
    <form method="post" action="/oauth/authorizationconfirmnewuser" class="requestAccount">
      <input class="shiny"
             name="email" type="email"
             infotext="$_FillInEmail()$"
             autocomplete="off" />
      <input type="hidden" value="$token$" name="oauth_token" />
      <a href="#" class="submit new-btn-small green2 center">
        <p>Create account</p>
      </a>
      <input type="submit" class="hidden" />
      <div class="clearfix" > </div>
    </form>
  </div>
  <div class="deny option right">
    <h2>Deny the privileges</h2>
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
###
####
apiDashboard =
<script>
var personalTokensTable = KontraList().init({
    name : "Personal Access Token",
    headerExtras: jQuery("<div class='middle'>").append(jQuery("<p>").append("$_userPersonalAccessToken()$")),
    emptyAlternative: 
      jQuery("<div >").append(jQuery("<div class='middle'>").append(jQuery("<p>").append("$_userPersonalAccessToken()$")))
                      .append(jQuery("<div class='middle'>").append(Button.init({
                      size: "small",
                      color: "green",
                      text: "Create Personal Token",
                      onClick: function() {
                        new Submit({
                          url: "/oauth/createpersonaltoken",
                          method: "POST",
                          ajax: true,
                          ajaxsuccess: function() {personalTokensTable.recall()}
                        }).send();
                      }
                    }).input())),
    schema: new Schema({
    url: "/oauth/dashboard/personaltoken",
    cells : [
        new Cell({name: "API Token",     width:"120px", field:"apitoken"}),
        new Cell({name: "API Secret",    width:"120px",  field:"apisecret"}),
        new Cell({name: "Access Token",  width:"120px",  field:"accesstoken"}),
        new Cell({name: "Access Secret", width:"120px",  field:"accesssecret"}),
        new Cell({name: "",              width:"70px",  field:"accesssecret", special: "rendered",
                 rendering: function(status,idx,listobject) {
                    var button = Button.init({
                      size: "small",
                      color: "red",
                      text: "Delete",
                      onClick: function() {
                        new Submit({
                          url: "/oauth/deletepersonaltoken",
                          method: "POST",
                          ajax: true,
                          ajaxsuccess: function() {personalTokensTable.recall()}
                        }).send();
                      }
                    });
                    return button.input();
                 }})
    ]})});

jQuery(function(){
    jQuery("#jspersonaltoken").append(personalTokensTable.view.el);
});

var grantedPrivilegesTable = KontraList().init({
    name : "Granted Privileges",
    emptyAlternative: jQuery("<div class='middle'>").append(jQuery("<p>").append("$_haveNoGrantedPrivileges()$")),
    headerExtras: jQuery("<div class='middle'>").append(jQuery("<p>").append("$_haveGrantedPrivileges()$")),
    schema: new Schema({
    url: "/oauth/dashboard/grantedprivileges",
    cells : [
        new Cell({name: "Company/Individual", width:"240px",  field:"name"}),
        new Cell({name: "Privilege",          width:"240px", field:"privilegedescription"}),
        new Cell({name: "",                   width:"70px",  field:"privilege", special: "rendered",
                 rendering: function(priv,idx,listobject) {
                    if(priv === "") {
                      return Button.init({
                        size: "small",
                        color: "red",
                        text: "Delete All",
                        onClick: function() {
                          new Submit({
                            tokenid: listobject.get("fields").tokenid,
                            url: "/oauth/deleteprivilege",
                            method: "POST",
                            ajax: true,
                            ajaxsuccess: function() {grantedPrivilegesTable.recall()}
                          }).send();
                        }
                      }).input();
                    } else {
                      return Button.init({
                        size: "small",
                        color: "red",
                        text: "Delete",
                        onClick: function() {
                          new Submit({
                            tokenid: listobject.get("fields").tokenid,
                            privilege: listobject.get("fields").privilege,
                            url: "/oauth/deleteprivilege",
                            method: "POST",
                            ajax: true,
                            ajaxsuccess: function() {grantedPrivilegesTable.recall()}
                          }).send();
                        }
                      }).input();
                    }
                 }})
    ]})});

jQuery(function(){
    jQuery("#jsgrantedprivileges").append(grantedPrivilegesTable.view.el);
});

var apiTokensTable = KontraList().init({
    name : "Granted Privileges",
    headerExtras: jQuery("<div />").append(jQuery("<div class='middle'>").append(jQuery("<p>").append("$_apiTokensMessage()$")))
                                   .append(jQuery("<div class='middle'>").append(Button.init({
                      size: "small",
                      color: "green",
                      text: "Create API Token",
                      onClick: function() {
                        new Submit({
                          url: "/oauth/createapitoken",
                          method: "POST",
                          ajax: true,
                          ajaxsuccess: function() {apiTokensTable.recall()}
                        }).send();
                      }
                    }).input())),
    schema: new Schema({
    url: "/oauth/dashboard/apitokens",
    cells : [
        new Cell({name: "API Token",  width:"240px",  field:"apitoken"}),
        new Cell({name: "API Secret", width:"240px", field:"apisecret"}),
        new Cell({name: "",           width:"70px",  field:"apitoken", special: "rendered",
                 rendering: function(apitoken,idx,listobject) {
                    return Button.init({
                      size: "small",
                      color: "red",
                      text: "Delete",
                      onClick: function() {
                        new Submit({
                          apitoken: apitoken,
                          url: "/oauth/deleteapitoken",
                          method: "POST",
                          ajax: true,
                          ajaxsuccess: function() {apiTokensTable.recall()}
                        }).send();
                      }
                    }).input();
                 }})
    ]})});

jQuery(function(){
    jQuery("#jsapitokens").append(apiTokensTable.view.el);
});

</script>
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
      <div id="jsgrantedprivileges"> </div>

      <h2>Personal Access Token</h2>
      <div id="jspersonaltoken"> </div>

      <h2>API Tokens</h2>
      <div id="jsapitokens"> </div>

    </div>
  </div>
</div>
####
