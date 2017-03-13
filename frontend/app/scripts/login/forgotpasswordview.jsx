var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Button = require("../common/button");
var InfoTextInput = require("../common/infotextinput");
var Track = require("../common/track");
var LoginModel = require("./loginmodel");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;


module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    sendPasswordReminderCallback : function(resp) {
      if (resp.maybe_sent == true) {
        // We do not want to tell the user with certainty, whether the email was sent or not.
        // The reason is this could be abused to find out whether an email is registered
        // with Scrive or not.
        new FlashMessage({ content: localization.loginModal.passwordReminderMaybeSend, type : "success"});
      } else {
        var text = "";
        if (resp.badformat == true) {
          text = localization.loginModal.invalidEmail;
        }
        Track.track('Error',{Message: 'password reminder failed: ' + text});
        new FlashMessage({ content: text, type : "error"});
      }
    },
    trySendPasswordReminder : function() {
      var self = this;
      this.props.model.sendPasswordReminder(function(r) {self.sendPasswordReminderCallback(r);});
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div>
          <div>
            <div className="position">
              <InfoTextInput
                infotext={localization.loginModal.email}
                value={model.email()}
                onChange={function(v) {model.setEmail(v);}}
                inputtype="text"
                name="email"
                onEnter={this.trySendPasswordReminder}
                autocomplete={true}
              />
            </div>
            <div className="position separated" >
              <Button
                cssClass="recovery-password-submit"
                type="main"
                text={localization.loginModal.sendNewPassword}
                onClick={this.trySendPasswordReminder}
              />
            </div>
            <div className="position separated">
              <div className='label-with-link'>
                <a onClick={function(){ model.goToLoginView();}}>
                  {localization.loginModal.login}
                </a>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
