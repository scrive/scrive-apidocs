var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DomainViewModel = require("./domainviewmodel");
var DomainView = require("./domainview");
var $ = require("jquery");


module.exports = React.createClass({
    propTypes: {
        domainid: React.PropTypes.string
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
    componentWillUnmount : function() {
      if (this.state.updateOnHashChangeFunction) {
        $(window).unbind('hashchange',this.state.updateOnHashChangeFunction);
      }
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var model = new DomainViewModel({
        domainid: props.domainid
      });
      return {model: model};
    },
    adjustToPageHash : function() {
      if (location.hash.startsWith("#branding-themes-email")) {
        this.state.model.switchToMailThemeMode();
      } else if (location.hash.startsWith("#branding-themes-signing-page")) {
        this.state.model.switchToSignviewThemeMode();
      } else if (location.hash.startsWith("#branding-themes-service")) {
        this.state.model.switchToServiceThemeMode();
      } else if (location.hash.startsWith("#branding-themes-login")) {
        this.state.model.switchToLoginThemeMode();
      } else if (location.hash.startsWith("#branding-settings")) {
        this.state.model.switchToAdditionalSettingsMode();
      }
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.state.model];
    },
    render: function() {
      return (
        <DomainView model={this.state.model}/>
      );
    }
  });
