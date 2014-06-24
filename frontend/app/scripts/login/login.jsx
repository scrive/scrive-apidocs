/** @jsx React.DOM */

define(['React', 'common/backbone_mixim', 'login/loginmodel','login/loginview', 'login/forgotpasswordview','legacy_code'], function(React, BackboneMixin, LoginModel, LoginView, ForgotPasswordView) {

return React.createClass({
    propTypes: {
        reminderView: React.PropTypes.bool,
        email : React.PropTypes.string,
        password : React.PropTypes.string,
        referer : React.PropTypes.string,
        autofocus: React.PropTypes.bool,
        pad : React.PropTypes.bool,
        servicelinkcolour : React.PropTypes.string,
        textscolour : React.PropTypes.string
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var model = new LoginModel({
        reminderView: props.reminderView,
        email : props.email,
        password : props.password,
        referer : props.referer,
        pad : props.pad,
        autofocus: props.autofocus,
        servicelinkcolour : props.servicelinkcolour,
        textscolour : props.textscolour
      });
      return {model: model};
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.state.model];
    },
    render: function() {
      return (
        <div>
         {/*if*/ (this.state.model.loginView() ) &&
            <LoginView model={this.state.model}/>
         }
         {/*else*/ (this.state.model.reminderView() ) &&
            <ForgotPasswordView model={this.state.model}/>
         }
        </div>
      );
    }
  });
});