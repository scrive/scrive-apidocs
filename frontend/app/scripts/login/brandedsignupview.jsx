define(['React','common/backbone_mixin','common/button','common/infotextinput', 'login/signupmodel','pages/languageselect','legacy_code'], function(React, BackboneMixin, Button, InfoTextInput, LoginModel, LanguageSelect) {


return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    componentDidMount: function () {
      var res = $("<div>" +  localization.signupModal.alreadyHaveAnAccount + "</div>");
      if (this.refs.loginLink) {
        $("a",res).click(function () {
          this.props.model.setView("login");
        }.bind(this));
        this.refs.loginLink.getDOMNode().appendChild(res[0]);
      }
    },
    trySignup: function() {
      this.props.model.signup();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className="signup-box" style={{width:"275px", margin:"20px auto"}}>
          <div style={{marginBottom:"20px", marginTop:"50px", textAlign: "center"}}>
            <img alt="logo" src="/login_logo/c97f12a8" />

            <div className="divider-line"></div>
            <span className="label" style={{textAlign:"center", width:"275px"}}>
              {localization.esigningpoweredbyscrive}
            </span>
          </div>

          <div>
            <div>
              <div>
                <div className="position first withEmail">
                  <InfoTextInput
                    infotext={localization.email}
                    value={model.email()}
                    onChange={function(v) {model.setEmail(v);}}
                    inputtype="text"
                    name="email"
                    onEnter={this.trySignup}
                    autocomplete={true}
                    inputStyle={{width : "245px", padding: "7px 14px"}}
                    style={{width : "273px", padding: "0px", fontSize : "16px"}}
                    focus={(model.email() == undefined || model.email() == "") && model.autofocus()}
                  />
                </div>

                <div className="position" style={{textAlign: "center", marginTop:"20px"}}>
                  <Button
                    type="main"
                    text={localization.signup}
                    style={{"width":"235px;"}}
                    onClick={this.trySignup}
                  />
                </div>

                {/*if*/ (!model.nolinks()) &&
                  <div className='position' style={{textAlign:"center",marginTop:"20px"}}>
                    <span ref="loginLink" className="label-with-link" />
                  </div>
                }
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
});
