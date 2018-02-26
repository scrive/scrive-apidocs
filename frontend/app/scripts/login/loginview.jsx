var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Button = require("../common/button");
var InfoTextInput = require("../common/infotextinput");
var LoginModel = require("./loginmodel");
var LanguageSelect = require("../pages/languageselect");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var $ = require("jquery");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var Track = require("../common/track");



module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    loginCallback : function(resp) {
      var model = this.props.model;
      if (resp.logged == true) {
        Track.track_timeout('Login successful', {}, function() {
          window.location = model.referer() != undefined && model.referer() != "" && model.referer() != "/" ? model.referer() : "/newdocument";
        });
      }
      else if( resp.totp_missing && !model.loginTotpView()) {
        model.goToLoginTotpView();
      }
      else if( resp.ipaddr ) {
        Track.track('Error',{
          Message: 'login failed due to IP restriction',
          IP: resp.ipaddr,
          Admin: resp.adminname
        });
        var text = $("<span>" + localization.loginModal.loginFailedBadIP + "</span>");
        $(".put-ip-here",text).text(resp.ipaddr);
        $(".put-adminname-here",text).text(resp.adminname);
        new FlashMessage({ content: text, type : "error"});
      }
      else if(
           (resp.totp_correct != undefined && !resp.totp_correct)
        || (model.loginTotpView() && resp.totp_missing != undefined && resp.totp_missing)
      ) {
        Track.track('Error',{
          Message: 'login failed due to incorrect TOTP'
        });
        new FlashMessage({ content: localization.loginModal.loginFailedBadTOTP, type : "error"});
      } else {
        Track.track('Error',{
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
      var emailProvided = model.email() != undefined && model.email() != "";
      return (
        <div>
          <div>
            {/*if*/ !model.loginTotpView() &&
            <div>
              <div className='position'>
                <InfoTextInput
                  infotext={localization.loginModal.email}
                  value={model.email()}
                  onChange={function(v) {model.setEmail(v);}}
                  inputtype="text"
                  name="email"
                  onEnter={this.tryLogin}
                  autocomplete={true}
                  focus={!emailProvided && model.autofocus()}
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
                  focus={emailProvided && model.autofocus()}
                  buttonTitle={localization.loginModal.forgot}
                  onButtonClick={function(){ model.goToReminderView();}}
                />
              </div>
            </div>
            }
            {/*else*/ model.loginTotpView() &&
            <div className='position'>
              <InfoTextInput
                infotext={localization.loginModal.twoFactor}
                value={model.totp()}
                onChange={function(v) {model.setTotp(v);} }
                inputtype="text"
                name="totp"
                onEnter={this.tryLogin}
                focus={emailProvided && model.autofocus()}
              />
            </div>
            }
            <div className="position separated">
              <Button
                type="main"
                text={localization.loginModal.login}
                onClick={this.tryLogin}
              />
            </div>
            {/*if*/ (!model.nolinks()) &&
              <div className="position separated">
                <HtmlTextWithSubstitution
                  className="label-with-link"
                  secureText={localization.loginModal.dontHaveAccount}
                  onClicks={{".put-link-to-signup-here": function () {  model.goToSignupView(); }}}
                />
              </div>
            }
          </div>
          <div className="position">
            <LanguageSelect
              langprefix={model.langprefix()}
              cssClass="change-language-for-login"
              width={198}
            />
          </div>
        </div>
      );
    }
  });
