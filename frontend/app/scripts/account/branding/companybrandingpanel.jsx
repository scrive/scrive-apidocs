var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CompanyBrandingViewModel = require("./companybrandingviewmodel");
var CompanyBrandingView = require("./companybrandingview");
var $ = require("jquery");


module.exports = React.createClass({
    propTypes: {
        companyid: React.PropTypes.string
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillMount : function() {
      var self = this;
      var updateOnHashChangeFunction = function() {self.adjustToPageHash();};
      $(window).bind('hashchange',updateOnHashChangeFunction);
      this.setState({updateOnHashChangeFunction : updateOnHashChangeFunction});
      self.adjustToPageHash();
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    componentWillUnmount : function() {
      if (this.state.updateOnHashChangeFunction) {
        $(window).unbind('hashchange',this.state.updateOnHashChangeFunction);
      }
    },
    stateFromProps : function(props) {
      var model = new CompanyBrandingViewModel({
        companyid: props.companyid
      });
      return {model: model};
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.state.model];
    },
    adjustToPageHash : function() {
      if (location.hash == "#branding-themes-email") {
        this.state.model.switchToMailThemeMode();
      } else if (location.hash == "#branding-themes-signing-page") {
        this.state.model.switchToSignviewThemeMode();
      } else if (location.hash == "#branding-themes-service") {
        this.state.model.switchToServiceThemeMode();
      } else if (location.hash == "#branding-settings") {
        this.state.model.switchToAdditionalSettingsMode();
      }
    },
    reload : function() {
      this.state.model.reload();
    },
    render: function() {
      return (
        <CompanyBrandingView model={this.state.model}/>
      );
    }
  });
