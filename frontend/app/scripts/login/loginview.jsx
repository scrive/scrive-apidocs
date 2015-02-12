/** @jsx React.DOM */

define(['React','common/backbone_mixin','common/button','common/infotextinput', 'login/loginmodel','pages/languageselect','legacy_code'], function(React, BackboneMixin, Button, InfoTextInput, LoginModel, LanguageSelect) {


return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    componentDidMount: function () {
      // Link is hidden inside a text - so we need to bind to it dynamically
      if (this.refs.signupLink != undefined) {
        var model = this.props.model;
        $("a",this.refs.signupLink.getDOMNode()).click(function () {
          model.goToSignupView();
        });
      }
    },
    loginCallback : function(resp) {
      var model = this.props.model;
      if (resp.logged == true) {
        trackTimeout('Login successful', {}, function() {
          window.location = model.referer() != undefined && model.referer() != "" && model.referer() != "/" ? model.referer() : "/newdocument";
        });
      }
      else if( resp.ipaddr ) {
        mixpanel.track('Error',{
          Message: 'login failed due to IP restriction',
          IP: resp.ipaddr,
          Admin: resp.adminname
        });
        var text = $("<span>" + localization.loginModal.loginFailedBadIP + "</span>");
        $(".put-ip-here",text).text(resp.ipaddr);
        $(".put-adminname-here",text).text(resp.adminname);
        new FlashMessage({ content: text, type : "error"});
      } else {
        mixpanel.track('Error',{
          Message: 'login failed'
        });
        new FlashMessage({ content: localization.loginModal.loginFailed, type : "error"});
      }
    },
    tryLogin : function() {
      var self = this;
      this.props.model.login(function(r) {self.loginCallback(r);});
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      return (
        <div>
          <div>
            <div className='position first' style={{marginBottom:"6px"}}>
              <InfoTextInput
                infotext={localization.loginModal.email}
                value={model.email()}
                onChange={function(v) {model.setEmail(v);}}
                inputtype="text"
                name="email"
                onEnter={this.tryLogin}
                autocomplete={true}
                inputStyle={{width : "245px", padding: "7px 14px"}}
                style={{width : "273px", padding: "0px", fontSize : "16px"}}
                focus={(model.email() == undefined || model.email() == "") && model.autofocus()}
              />
            </div>
            <div className='position'>
              <InfoTextInput
                infotext={localization.loginModal.password}
                value={model.password()}
                onChange={function(v) {model.setPassword(v);} }
                inputtype="password"
                name="password"
                onEnter={this.tryLogin}
                style={{width : "245px", padding : "7px 14px", fontSize : "16px" }}
                focus={!(model.email() == undefined || model.email() == "") && model.autofocus()}
                buttonTitle="Forgot?"
                onClick={function(){ model.goToReminderView();}}
              />
            </div>
            <div className="position" style={{textAlign:"center",marginTop:"20px"}}>
              <Button
                type="main"
                text={localization.loginModal.login}
                style={{"width":"235px;"}}
                onClick={this.tryLogin}
              />
            </div>
            {/*if*/ (!model.nolinks()) &&
              <div className='position' style={{textAlign:"center",marginTop:"20px"}}>
                <span
                  ref="signupLink"
                  className='label-with-link'
                  dangerouslySetInnerHTML={{__html:localization.loginModal.dontHaveAccount}}
                >
                </span>
              </div>
            }
          </div>
          <div className="position" style={{textAlign:"center",marginTop:"20px",width: "200px", marginRight: "auto", marginLeft: "auto"}}>
            <LanguageSelect
              langprefix={model.langprefix()}
              border=""
              cssClass="change-language-for-login"
              optionsWidth="198px"
              textWidth={171}
            />
          </div>
        </div>
      );
    }
  });
});
