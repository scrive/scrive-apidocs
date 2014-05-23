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
        <div style={{"width":"275px","margin" : "20px auto"}}>
          <div style={{marginBottom: "103px", textAlign: "center"}} >
            <img alt='logo' src={model.logolink()} />
            <div className='divider-line'/>
            <label style={{"textAlign":"center", "width":"275px", color : model.textscolour()}}>
              {localization.esigningpoweredbyscrive}
            </label>
          </div>
          <div>
            <div>
              <div className='position first' style={{textAlign: "left", height: "30px"}} >
                <label style={{paddingLeft: "10px", color : model.textscolour()}}>
                  {localization.resetYourPassword  + ":"}
                </label>
              </div>
              <div className='position' style={{marginBottom:"6px"}}>
                <InfoTextInput
                  infotext={localization.loginModal.email}
                  value={model.email()}
                  onChange={function(v) {model.setEmail(v);}}
                  inputtype="text"
                  name="email"
                  onEnter={this.trySendPasswordReminder}
                  autocomplete={true}
                  style={{"width" : "245px", "padding" : "7px 14px","fontSize" : "16px"}}
                />
              </div>
              <div className="position" style={{textAlign:"center", marginTop:"10px"}}>
                <Button
                  size="tiny"
                  cssClass="recovery-password-submit"
                  color={model.buttoncolorclass()}
                  text={localization.loginModal.sendNewPassword}
                  style={{"width":"245px;"}}
                  onClick={this.trySendPasswordReminder}
                />
              </div>
            </div>
          </div>
        </div>
      );
    }
  });

});
