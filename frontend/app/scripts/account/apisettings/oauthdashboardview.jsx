var Backbone = require("backbone");
var React = require("react");
var List = require("../../lists/list");
var CallbackScheme = require("./callbackscheme");
var CallbackSchemeView = require("./callbackschemeview");
var Button = require("../../common/button");
var Submit = require("../../../js/submits.js").Submit;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;


module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    propTypes : {
      loadLater: React.PropTypes.bool
    },
    componentDidMount: function () {
      if (this.props.loadLater === false) {
        this.reload();
      }
    },
    deleteApiToken : function(apitoken) {
      var self = this;
      new Submit({
        apitoken: apitoken,
        url: "/oauth/deleteapitoken",
        method: "POST",
        ajax: true,
        ajaxsuccess: function() {self.refs.apitokenlist.reload();}
     }).send();
    },
    createApiToken : function() {
      var self = this;
      new Submit({
        url: "/oauth/createapitoken",
        method: "POST",
        ajax: true,
        ajaxsuccess: function() {self.refs.apitokenlist.reload();}
     }).send();
    },
    deleteGrantedPrivilige : function(tokenid,privilege) {
      var self = this;
      new Submit({
        tokenid: tokenid,
        privilege: privilege,
        url: "/oauth/deleteprivilege",
        method: "POST",
        ajax: true,
        ajaxsuccess: function() {self.refs.grantedprivilegeslist.reload();}
      }).send();
    },
    deletePersonalToken : function() {
      var self = this;
      new Submit({
        url: "/oauth/deletepersonaltoken",
        method: "POST",
        ajax: true,
        ajaxsuccess: function() {self.refs.personaltokenlist.reload();}
      }).send();
    },
    createPersonalToken : function() {
      var self = this;
      new Submit({
        url: "/oauth/createpersonaltoken",
        method: "POST",
        ajax: true,
        ajaxsuccess: function() {self.refs.personaltokenlist.reload();}
      }).send();
    },
    copyPersonalCredentialsToClipboard: function(token) {
      var credentialsAsString =
        "API token: " + token.field("apitoken") + ", " +
        "API secret: " + token.field("apisecret") + ", " +
        "access token: " + token.field("accesstoken") + ", " +
        "access secret: " + token.field("accesssecret");

      if (navigator.clipboard) {
        navigator.clipboard.writeText(credentialsAsString);
      } else {
        var fallbackCopyTextToClipboard = function (text) {
          var textArea = document.createElement("textarea");
          textArea.style.position = 'fixed';
          textArea.style.top = 0;
          textArea.style.left = 0;
          textArea.style.opacity = 0;
          textArea.value = text;
          document.body.appendChild(textArea);
          textArea.focus();
          textArea.select();
          document.execCommand('copy');
          document.body.removeChild(textArea);
        }
        fallbackCopyTextToClipboard(credentialsAsString);
      }

      new FlashMessage({
        type: "success",
        content: localization.apiDashboard.personalTokenCopySucceeded,
        hideTimeout: 2000
        });
    },
    render: function() {
      var self = this;
      return (
        <div className="apidashboard">
          <div className="oauth-section">
            <div className="oauth-section-header">{localization.apiDashboard.grandedTokens}</div>
            <div className="oauth-section-label">{localization.apiDashboard.grantedPriviligesDescription}</div>
            <div>
              <List.List
                ref='grantedprivilegeslist'
                url="/oauth/dashboard/grantedprivileges"
                loadLater={true}
                dataFetcher={function(d) {return d.granted_privileges;}}
              >
                <List.Column
                  name={localization.apiDashboard.personOrCompany}
                  width="240px"
                  rendering={function(d) {
                    return (<div>{d.field("name")}</div>);
                  }}
                />
                <List.Column
                  name={localization.apiDashboard.privilige}
                  width="240px"
                  rendering={function(d) {
                    return (<div>{d.field("privilegedescription")}</div>);
                  }}
                />
                <List.Column
                  name=""
                  width="70px"
                  rendering={function(d) {
                    return (
                      <label className='oauth-delete-token' onClick={function() {self.deleteGrantedPrivilige(d.field("tokenid"),d.field("privilege"))}}>
                        {localization.apiDashboard.deleteToken}
                      </label>);
                  }}
                />
              </List.List>
            </div>
          </div>
          <div className="oauth-section">
            <div className="oauth-section-header">{localization.apiDashboard.personalTokens}</div>
            <div className="oauth-section-label">{localization.apiDashboard.personalTokenDescription}</div>
            <div>
              <List.List
                ref='personaltokenlist'
                url="/oauth/dashboard/personaltoken"
                loadLater={true}
                dataFetcher={function(d) {return d.personal_tokens;}}
              >
                <List.ListHeader availableWhen={function(model) {
                  return model.ready() && model.list().length != 0 && !model.list().at(0).field("hidden");}}
                >
                  <div className="oauth-section-warning">{localization.apiDashboard.personalTokenWarning}</div>
                </List.ListHeader>
                <List.Column
                  name="Client credentials identifier"
                  width="120px"
                  rendering={function(d) {
                    return (<div>{d.field("apitoken")}</div>);
                  }}
                />
                <List.Column
                  name="Client credentials secret"
                  width="120px"
                  rendering={function(d) {
                    return (<div>{d.field("apisecret")}</div>);
                  }}
                />
                <List.Column
                  name="Token credentials identifier"
                  width="120px"
                  rendering={function(d) {
                    return (<div>{d.field("accesstoken")}</div>);
                  }}
                />
                <List.Column
                  name="Token credentials secret"
                  width="120px"
                  rendering={function(d) {
                    return (<div>{d.field("accesssecret")}</div>);
                  }}
                />
                <List.Column
                  name=""
                  width="20px"
                  rendering={function(d) {
                    if (!d.field("hidden")) {
                    return (
                      <div className='oauth-copy-token'
                        onClick={function() {self.copyPersonalCredentialsToClipboard(d)}}
                        title={localization.apiDashboard.personalTokenCopyTooltip}
                      >
                      </div>);
                    } else {
                      return (<div></div>);
                    }
                  }}
                />
                <List.Column
                  name=""
                  width="70px"
                  rendering={function(d) {
                    return (
                      <label className='oauth-delete-token' onClick={function() {self.deletePersonalToken()}}>
                        {localization.apiDashboard.deleteToken}
                      </label>);
                  }}
                />
                <List.ListFooter
                  availableWhen={function(model) {return model.ready() && model.list().length == 0;}}
                >
                  <div style={{"marginLeft":"1px","marginTop":"12px", "marginBottom":"5px"}}>
                    <Button
                      size="tiny"
                      type="action"
                      text={localization.apiDashboard.personalTokenCreate}
                      onClick={function() {self.createPersonalToken();}}
                    />
                  </div>
                </List.ListFooter>
              </List.List>
            </div>
          </div>
          <div className="oauth-section">
            <div className="oauth-section-header">{localization.apiDashboard.apiTokens}</div>
            <div className="oauth-section-label">{localization.apiDashboard.apiTokenDescription}</div>
            <div>

             <List.List
              ref='apitokenlist'
              url="/oauth/dashboard/apitokens"
              idFetcher={function(d) {return d.field("apitoken");}}
              loadLater={true}
              dataFetcher={function(d) {return d.api_tokens;}}
             >
              <List.Column
                name="Client credentials identifier"
                width="240px"
                rendering={function(d) {
                  return (<div>{d.field("apitoken")}</div>);
                }}
              />
              <List.Column
                name="Client credentials secret"
                width="240px"
                rendering={function(d) {
                  if (d.getProperty("hidden"))
                    return (<div>{d.field("apisecret")}</div>);
                  else
                    return (
                      <span
                        onClick={function() {d.setProperty("hidden",true)}}
                        className="oauth-hidden-token"
                      >
                        {localization.apiDashboard.reveal}
                      </span>
                    );
                }}
              />
              <List.Column
                name=""
                width="70px"
                rendering={function(d) {
                  return (
                    <label className='oauth-delete-token' onClick={function() {self.deleteApiToken(d.field("apitoken"))}}>
                      {localization.apiDashboard.deleteToken}
                    </label>);
                }}
              />
              <List.ListFooter>
                <div style={{"marginLeft":"1px","marginTop":"12px", "marginBottom":"5px"}}>
                  <Button
                    size="tiny"
                    type="action"
                    text={localization.apiDashboard.apiTokenCreate}
                    onClick={function() {self.createApiToken();}}
                  />
                </div>
              </List.ListFooter>
            </List.List>

            </div>
          </div>
          <div className="oauth-section">
            <CallbackSchemeView
              ref='callbackschemeview'
              model={new CallbackScheme({})}
            />
          </div>
        </div>);
      }


});
