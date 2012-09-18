####
apiDashboard =
<script>
var personalTokensTable = KontraList().init({
    name : "Personal Access Token",
    headerExtras: jQuery("<div class='middle'>").append(jQuery("<p>").append(localization.apiDashboard.personalTokenDescription)),
    emptyAlternative: 
      jQuery("<div style='height:28px;padding:2px;'>")
                      .append(jQuery("<div class='float-right'>").append(Button.init({
                      size: "tiny",
                      color: "green",
                      text: localization.apiDashboard.personalTokenCreate,
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
        new Cell({name: "Client credentials identifier",     width:"120px", field:"apitoken"}),
        new Cell({name: "Client credentials secret",    width:"120px",  field:"apisecret"}),
        new Cell({name: "Token credentials identifier",  width:"120px",  field:"accesstoken"}),
        new Cell({name: "Token credentials secret", width:"120px",  field:"accesssecret"}),
        new Cell({name: "",              width:"70px",  field:"accesssecret", special: "rendered",
                 rendering: function(status,idx,listobject) {
                    var button = Button.init({
                      size: "tiny",
                      color: "red",
                      text: localization.apiDashboard.delete,
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
    headerExtras: jQuery("<div class='middle'>").append(jQuery("<p>").append(localization.apiDashboard.grantedPriviligesDescription)),
    schema: new Schema({
    url: "/oauth/dashboard/grantedprivileges",
    cells : [
        new Cell({name: localization.apiDashboard.personOrCompany, width:"240px",  field:"name"}),
        new Cell({name: localization.apiDashboard.privilige,          width:"240px", field:"privilegedescription"}),
        new Cell({name: "",                   width:"70px",  field:"privilege", special: "rendered",
                 rendering: function(priv,idx,listobject) {
                    if(priv === "") {
                      return Button.init({
                        size: "tiny",
                        color: "red",
                        text: localization.apiDashboard.deleteAll,
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
                        size: "tiny",
                        color: "red",
                        text: localization.apiDashboard.delete,
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
    headerExtras: jQuery("<div />").append(jQuery("<div class='middle'>").append(jQuery("<p>").append(localization.apiDashboard.apiTokenDescription)))
                                   .append(jQuery("<div class='middle'>").append(Button.init({
                      size: "tiny",
                      color: "green",
                      text: localization.apiDashboard.apiTokenCreate,
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
        new Cell({name: "Client credentials identifier",  width:"240px",  field:"apitoken"}),
        new Cell({name: "Client credentials secret", width:"240px", field:"apisecret"}),
        new Cell({name: "",           width:"70px",  field:"apitoken", special: "rendered",
                 rendering: function(apitoken,idx,listobject) {
                    return Button.init({
                      size: "tiny",
                      color: "red",
                      text: localization.apiDashboard.delete,
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
    <li class="float-left"><a href="/account">$_HeadUserInfo()$</a></li>
    <li class="float-left"><a href="/account/security">$_HeadAccountSettings()$</a></li>
    $if(iscompanyadmin)$<li class="float-left"><a href="/account/company">$_CompanySettings()$</a></li>$endif$
    $if(iscompanyadmin)$<li class="float-left"><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
    <li class="float-left"><a href="/account/mailapi">$_HeadMailApi()$</a></li>
    <li class="float-left"><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
    <li class="active float-left"><a href="/oauth/dashboard">API</a></li>
    <li class="float-right"/>
  </ul>
  <div class="tab-container">
    <div class="tab-content account">
      <div class='oauth-section'>
        <h2 class="first">$_apiDashboardGrantedPriviliges()$</h2>
        <div id="jsgrantedprivileges"> </div>
      </div>
      <div class='oauth-section'>
        <h2>$_apiDashboardPersonalToken()$</h2>
        <div id="jspersonaltoken"> </div>
      </div>
      <div class='oauth-section'>
        <h2>$_apiDashboardApiToken()$</h2>
        <div id="jsapitokens"> </div>
      </div>
    </div>
  </div>
</div>
####
