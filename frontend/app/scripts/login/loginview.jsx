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
        <div className="short-input-section">
          <div className='shadowed'>
            <h1>
              {localization.welcomeback}
            </h1>
          </div>
          <div className='short-input-container login'>
            <div className='short-input-container-body-wrapper'>
              <div className='short-input-container-body'>
                <div className='position first'>
                  <InfoTextInput
                    infotext = {localization.loginModal.email}
                    value = {model.email()}
                    onChange = {function(v) {model.setEmail(v);}}
                    className = "big-input"
                    style = {{padding:"0px",width:"310px"}}
                    inputStyle = {{"padding": "14px", "width" : "282px"}}
                    inputtype = "text"
                    name = "email"
                    autocomplete = {true}
                    onEnter = {this.tryLogin}
                    focus={(model.email() == undefined || model.email() == "") && model.autofocus()}
                  />
                </div>
                <div className='position'>
                  <InfoTextInput
                    infotext= {localization.loginModal.password}
                    value = {model.password()}
                    onChange = {function(v) {model.setPassword(v);}}
                    inputtype = "password"
                    className = "big-input"
                    name = "password"
                    onEnter = {this.tryLogin}
                    focus={!(model.email() == undefined || model.email() == "") && model.autofocus()}
                  />
                </div>
                <div className="position">
                   <Button
                     size  = "small"
                     color = "green"
                     text  = {localization.loginModal.login}
                     cssClass = "login-button "
                     onClick = {this.tryLogin}
                   />
                </div>
              </div>
              <div className='short-input-container-footer'>
                <p className='float-right'>
                  <a href='#' className="s-forgot-password" onClick={function(){ model.toogleView();}}>
                    {localization.loginModal.forgotpassword}
                  </a>
                </p>
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
});
