define(['React','common/backbone_mixin','common/button','common/infotextinput', 'common/hubspot_service', 'common/adwords_conversion_service', 'legacy_code'], function(React, BackboneMixin, Button, InfoTextInput, HubSpot, AdwordsConversionService) {


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
    signupCallback : function(resp) {
    var model = this.props.model;
      if (resp.sent === true) {
        _gaq.push(['_trackEvent', 'Signup', 'Clicked']);
        AdwordsConversionService.markAsSignupConversion();
        mixpanel.track('Create new account', {'Email' : model.email()});
        mixpanel.people.set({'$email' : model.email() });
        HubSpot.track(HubSpot.FORM_SIGNUP, {
          email :  model.email(),
          language : Language.current(),
          scrive_domain : location.hostname,
          signup_method : "AccountRequest"
        });
        var content = localization.payments.outside.confirmAccountCreatedUserHeader;
        new FlashMessage({content: content, type: 'success'});
      } else if (resp.sent === false) {
        mixpanel.track('Error', {Message : 'signup failed'});
        new FlashMessage({content: localization.accountSetupModal.flashMessageUserAlreadyActivated, type: 'error'});
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
