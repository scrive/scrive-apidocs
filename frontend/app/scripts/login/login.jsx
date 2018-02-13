var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var LoginModel = require("./loginmodel");
var LoginView = require("./loginview");
var ForgotPasswordView = require("./forgotpasswordview");
var SignupView = require("./signupview");
var $ = require("jquery");


module.exports = React.createClass({
    propTypes: {
        view: React.PropTypes.string,
        defaultView: React.PropTypes.string,
        email : React.PropTypes.string,
        password : React.PropTypes.string,
        referer : React.PropTypes.string,
        autofocus: React.PropTypes.bool,
        pad : React.PropTypes.bool,
        nolinks : React.PropTypes.string,
        langprefix : React.PropTypes.string
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var model = new LoginModel({
        view: props.view,
        defaultView: props.defaultView,
        email : props.email,
        password : props.password,
        referer : props.referer,
        pad : props.pad,
        autofocus: props.autofocus,
        nolinks : props.nolinks,
        langprefix : props.langprefix
      });
      return {model: model};
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.state.model];
    },
    componentWillMount : function() {
      // We first choose a view that matches a hash.
      this.synchHashWithModel();
      // And then make sure that hash actually matches the view
      this.synchModelWithHash();
    },
    componentDidMount : function() {
      var self = this;
      $(window).hashchange(function() {
        self.synchHashWithModel();
      });
    },
    componentWillUnmount : function() {
      $(window).unbind("hashchange");
    },
    componentWillUpdate : function() {
      this.synchModelWithHash();
    },
    synchModelWithHash : function() {
      if (window.location.hash != "#log-in" && this.state.model.loginView()) {
        window.location.hash = "#log-in";
      } else if (window.location.hash != "#sign-up" && this.state.model.signupView()) {
        window.location.hash = "#sign-up";
      } else if (window.location.hash != "#forgot" && this.state.model.reminderView()) {
        window.location.hash = "#forgot";
      }
    },
    synchHashWithModel : function() {
      if (window.location.hash == "#log-in" && !this.state.model.loginView()) {
        this.state.model.goToLoginView();
      } else if (window.location.hash == "#sign-up" && !this.state.model.signupView()) {
        this.state.model.goToSignupView();
      } else if (window.location.hash == "#forgot" && !this.state.model.reminderView()) {
        this.state.model.goToReminderView();
      }
    },
    render: function() {
      return (
        <div className="login-box">
          <div className="logo-wrapper">
            <img alt='logo' src={window.cdnbaseurl + "/login_logo/" + window.brandingdomainid + "/" + window.brandinghash} />
            <div className='divider-line'/>
            <div className='label'>
              {localization.esigningpoweredbyscrive}
            </div>
          </div>
          {/* if */   (this.state.model.loginView()) &&
            (<LoginView model={this.state.model}/>)
          }
          {/* else */ (this.state.model.reminderView()) &&
            (<ForgotPasswordView model={this.state.model}/>)
          }
          {/* else */ (this.state.model.signupView()) &&
            (<SignupView model={this.state.model}/>)
          }
        </div>
      );
    }
  });
