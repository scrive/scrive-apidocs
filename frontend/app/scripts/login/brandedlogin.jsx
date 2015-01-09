/** @jsx React.DOM */

define(['React', 'common/backbone_mixin', 'login/loginmodel','login/brandedloginview', 'login/brandedforgotpasswordview','legacy_code'], function(React, BackboneMixin, LoginModel, BrandedLoginView, BrandedForgotPasswordView) {

return React.createClass({
    propTypes: {
        reminderView: React.PropTypes.bool,
        email : React.PropTypes.string,
        password : React.PropTypes.string,
        referer : React.PropTypes.string,
        autofocus: React.PropTypes.bool,
        pad : React.PropTypes.bool,
        nolink : React.PropTypes.string,
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
        reminderView: props.reminderView,
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
    render: function() {
      return (
        <div>
         {/*if*/ (this.state.model.loginView() ) &&
            <BrandedLoginView model={this.state.model}/>
         }
         {/*else*/ (this.state.model.reminderView() ) &&
            <BrandedForgotPasswordView model={this.state.model}/>
         }
        </div>
      );
    }
  });
});
