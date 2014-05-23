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
    toLinkWithColor : function(text) {
      var res = $("<div>" + text + "</div>");
      $("a",res).css("color",this.props.model.servicelinkcolour())
      return res.html();
    },
    tryLogin : function() {
      this.props.model.login();
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      return (
        <div style={{"width":"275px","margin" : "20px auto"}}>
          <div style={{marginBottom: "50px", marginTop: "50px", textAlign: "center"}} >
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
                  {localization.login + ":"}
                </label>
              </div>
              <div className='position' style={{marginBottom:"6px"}}>
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
                  focus={model.email() == "" && model.autofocus()}
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
                  focus={model.email() != "" && model.autofocus()}
                />
              </div>
              <div className='position' style={{textAlign:"left",height:"30px"}}>
                <div style={{display:"inline-block",width:"224px",textAlign:"left",verticalAlign: "bottom", marginLeft: "4px"}}>
                  <label className="s-forgot-password"
                         style={{borderBottom: "1px solid #999999",color:"#999999",fontStyle:"italic",fontSize:"10px",lineHeight: "12px", color: model.textscolour()}}
                         onClick={function(){ model.toogleView();}}
                  >
                   {localization.loginModal.forgotpassword}
                  </label>
                </div>
              </div>
              <div className="position" style={{textAlign:"center"}}>
                <Button
                  size="tiny"
                  color={model.buttoncolorclass()}
                  text={localization.loginModal.login}
                  style={{"width":"245px;"}}
                  onClick={this.tryLogin}
                />
              </div>
              <div className='position' style={{textAlign:"center",marginTop:"20px"}}>
               <label className='label-with-link'
                      style={{color:model.textscolour()}}
                      dangerouslySetInnerHTML={{__html: this.toLinkWithColor(localization.loginModal.dontHaveAccount)}}
               />
               <label className='label-with-link'
                      style={{color:model.textscolour()}}
                      dangerouslySetInnerHTML={{__html: this.toLinkWithColor(localization.visitOurPricingPage)}}
               />
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
});