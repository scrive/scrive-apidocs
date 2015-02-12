define(['React','common/backbone_mixin','common/button','common/infotextinput', 'pages/languageselect','legacy_code'], function(React, BackboneMixin, Button, InfoTextInput) {


return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    componentDidMount: function () {
      // Link is hidden inside a text - so we need to bind to it dynamically
      if (this.refs.loginLink != undefined) {
        var model = this.props.model;
        $("a",this.refs.loginLink.getDOMNode()).click(function () {
          model.goToLoginView();
        });
      }
    },
    trySignup: function() {
      this.props.model.signup();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
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
                  <span
                    ref="loginLink"
                    className="label-with-link"
                    dangerouslySetInnerHTML={{__html:localization.signupModal.alreadyHaveAnAccount}}
                  >
                  </span>
                </div>
              }
            </div>
          </div>
        </div>
      );
    }
  });
});
