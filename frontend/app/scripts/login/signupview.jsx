var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Button = require("../common/button");
var InfoTextInput = require("../common/infotextinput");
var HubSpot = require("../common/hubspot_service");
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var Track = require("../common/track");
var Language = require("../../js/utils/language.js").Language;
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var EmailValidation = require("../../js/validation.js").EmailValidation;


module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    signupCallback : function(resp) {
      var model = this.props.model;
      if (resp.sent === true) {
        // Even if account was not created, we do not want to tell the user.
        // The reason is this could be abused to find out whether email is registered
        // with Scrive or not.
        var content = localization.payments.outside.confirmAccountMaybeCreatedUser;
        new FlashMessage({content: content, type: 'success'});
      }
    },
    trySignup: function() {
     var self = this;
     if (new EmailValidation({}).validateData(this.props.model.email())) {
       this.props.model.signup(function(r) { self.signupCallback(r);});
     } else {
       new FlashMessage({content: localization.validation.wrongEmail, type: 'error'});
     }
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div>
          <div>
            <div>
              <div className="position">
                <InfoTextInput
                  infotext={localization.email}
                  value={model.email()}
                  onChange={function(v) {model.setEmail(v);}}
                  inputtype="text"
                  name="email"
                  onEnter={this.trySignup}
                  autocomplete={true}
                  focus={(model.email() == undefined || model.email() == "") && model.autofocus()}
                />
              </div>

              <div className="position separated">
                <Button
                  type="main"
                  text={localization.signup}
                  onClick={this.trySignup}
                />
              </div>

              {/*if*/ (!model.nolinks()) &&
                <div className='position separated'>
                  <HtmlTextWithSubstitution
                    className="label-with-link"
                    secureText={localization.signupModal.alreadyHaveAnAccount}
                    onClicks={{".put-link-to-login-here": function () {  model.goToLoginView(); }}}
                  />
                </div>
              }
            </div>
          </div>
        </div>
      );
    }
  });
