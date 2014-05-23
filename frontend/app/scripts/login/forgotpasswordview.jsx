/** @jsx React.DOM */

define(['React','common/backbone_mixim','common/button','common/infotextinput', 'login/loginmodel' ,'legacy_code'], function(React, BackboneMixin, Button, InfoTextInput, LoginModel) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    trySendPasswordReminder : function() {
      this.props.model.sendPasswordReminder();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className="short-input-section">
          <div className='shadowed recovery'>
            <h1>
              {localization.resetYourPassword + "."}
            </h1>
            <h2>
              {localization.resetYourPasswordCheckEmail}
            </h2>
          </div>

          <div className='short-input-container recovery'>
            <div className='short-input-container-body-wrapper'>
              <div className='short-input-container-body'>
                <div className='position first'>
                  <InfoTextInput
                    infotext = {localization.loginModal.email}
                    value = {model.email()}
                    onChange = {function(v) {model.setEmail(v);}}
                    className = "big-input"
                    inputtype = "text"
                    name = "email"
                    autocomplete = {true}
                    onEnter = {function() {model.sendPasswordReminder();}}
                    focus={true}
                  />
                  <Button
                    size="big"
                    color = "green"
                    text  = {localization.loginModal.sendNewPassword}
                    cssClass ="recovery-password-submit"
                    onClick  = {function() {model.sendPasswordReminder();}}
                  />
                </div>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });

});
