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
      var res = $("<div>" + localization.loginModal.dontHaveAccount + "</div>");
      if (this.refs.signupLink) {
        $("a",res).click(function () {
          this.props.model.setView("signup");
        }.bind(this));
        this.refs.signupLink.getDOMNode().appendChild(res[0]);
      }
    },
    tryLogin : function() {
      this.props.model.login();
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      return (
        <div className="login-box" style={{"width":"275px","margin" : "20px auto"}}>
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
                  buttonTitle={localization.loginModal.forgot}
                  onClick={function(){ model.setView("reminder");}}
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
                  <span ref="signupLink" className='label-with-link' />
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
        </div>
      );
    }
  });
});
