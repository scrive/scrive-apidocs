/** @jsx React.DOM */


define(['Backbone', 'React', 'lists/list', 'account/apisettings/callbackscheme', 'account/apisettings/callbackschemeview','legacy_code','common/button'], function(Backbone, React, List, CallbackScheme, CallbackSchemeView,_legacy,Button) {

return React.createClass({
    mixins : [List.ReloadableContainer],
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
    render: function() {
      var self = this;
      return (
        <div className="tab-container apidashboard">
          <div className="oauth-section">
            <label className="oauth-section-header">{localization.apiDashboard.grandedTokens}</label>
            <label className="oauth-section-label">{localization.apiDashboard.grantedPriviligesDescription}</label>
            <div>
              <List.List
                ref='grantedprivilegeslist'
                url="/oauth/dashboard/grantedprivileges"
                dataFetcher={function(d) {return d.list;}}
              >
                <List.Column
                  name={localization.apiDashboard.personOrCompany}
                  width="240px"
                  rendering={function(d) {
                    return (<div>{d.field("fields").name}</div>);
                  }}
                />
                <List.Column
                  name={localization.apiDashboard.privilige}
                  width="240px"
                  rendering={function(d) {
                    return (<div>{d.field("fields").privilegedescription}</div>);
                  }}
                />
                <List.Column
                  name=""
                  width="70px"
                  rendering={function(d) {
                    return (
                      <label className='oauth-delete-token' onClick={function() {self.deleteGrantedPrivilige(d.field("fields").tokenid,d.field("fields").privilege)}}>
                        {localization.apiDashboard.deleteToken}
                      </label>);
                  }}
                />
              </List.List>
            </div>
          </div>
          <div className="oauth-section">
            <label className="oauth-section-header">{localization.apiDashboard.personalTokens}</label>
            <label className="oauth-section-label">{localization.apiDashboard.personalTokenDescription}</label>
            <div>
              <List.List
                ref='personaltokenlist'
                url="/oauth/dashboard/personaltoken"
                dataFetcher={function(d) {return d.list;}}
              >
                <List.Column
                  name="Client credentials identifier"
                  width="120px"
                  rendering={function(d) {
                    return (<div>{d.field("fields").apitoken}</div>);
                  }}
                />
                <List.Column
                  name="Client credentials secret"
                  width="120px"
                  rendering={function(d) {
                      if (d.getProperty("hiddenSecret"))
                        return (<div>{d.field("fields").apisecret}</div>);
                      else
                        return (
                          <span
                            onClick={function() {d.setProperty("hiddenSecret",true)}}
                            className="oauth-hidden-token"
                          >
                            {localization.apiDashboard.reveal}
                          </span>
                        );
                  }}
                />
                <List.Column
                  name="Token credentials identifier"
                  width="120px"
                  rendering={function(d) {
                    return (<div>{d.field("fields").accesstoken}</div>);
                  }}
                />
                <List.Column
                  name="Token credentials secret"
                  width="120px"
                  rendering={function(d) {
                      if (d.getProperty("hiddenAccess"))
                        return (<div>{d.field("fields").accesssecret}</div>);
                      else
                        return (
                          <span
                            onClick={function() {d.setProperty("hiddenAccess",true)}}
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
            <label className="oauth-section-header">{localization.apiDashboard.apiTokens}</label>
            <label className="oauth-section-label">{localization.apiDashboard.apiTokenDescription}</label>
            <div>

             <List.List
              ref='apitokenlist'
              url="/oauth/dashboard/apitokens"
              idFetcher={function(d) {return d.field("fields").apitoken;}}
              dataFetcher={function(d) {return d.list;}}
             >
              <List.Column
                name="Client credentials identifier"
                width="240px"
                rendering={function(d) {
                  return (<div>{d.field("fields").apitoken}</div>);
                }}
              />
              <List.Column
                name="Client credentials secret"
                width="240px"
                rendering={function(d) {
                  if (d.getProperty("hidden"))
                    return (<div>{d.field("fields").apisecret}</div>);
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
                    <label className='oauth-delete-token' onClick={function() {self.deleteApiToken(d.field("fields").apitoken)}}>
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
            <CallbackSchemeView model={new CallbackScheme({})}/>
          </div>
        </div>);
      }


});

});

