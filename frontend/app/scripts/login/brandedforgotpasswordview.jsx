/** @jsx React.DOM */

define(['React','common/backbone_mixin','common/button','common/infotextinput', 'login/loginmodel' ,'legacy_code'], function(React, BackboneMixin, Button, InfoTextInput, LoginModel) {

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
        <div className="forgot-password-box" style={{"width":"275px","margin" : "20px auto"}}>
          <div style={{marginBottom: "20px", marginTop: "50px", textAlign: "center"}} >
            <img alt='logo' src={"/login_logo/" + window.brandinghash} />
            <div className='divider-line'/>
            <div className='label' style={{"textAlign":"center", "width":"275px"}}>
              {localization.esigningpoweredbyscrive}
            </div>
          </div>
          <div>
            <div>
              <div className='position first' style={{marginBottom:"6px"}}>
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
              <div className="position" style={{textAlign:"center", marginTop:"20px"}}>
                <Button
                  cssClass="recovery-password-submit"
                  type="main"
                  text={localization.loginModal.sendNewPassword}
                  style={{"width":"235px;"}}
                  onClick={this.trySendPasswordReminder}
                />
              </div>
              <div className='position' style={{textAlign:"center",marginTop:"20px"}}>
                <div className='label-with-link'>
                  <a onClick={function(){ model.toogleView();}}>
                    {localization.loginModal.login}
                  </a>
                </div>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });

});
