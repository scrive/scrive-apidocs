/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin', 'to-start/field'], function(_legacy, React, BackboneMixin, ToStartField) {

return React.createClass({
  propTypes: {
    signatory: React.PropTypes.object,
    signviewbranding: React.PropTypes.object
  },
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels: function() {
    return [this.props.signatory];
  },
  getInitialState: function() {
    var allNonSignatureFieldsReadOnly = _.all(this.props.signatory.fields(), function(field) {
      if (field.isSignature()) return true;

      return field.isAuthorUnchangeableField();
    });

    return {'minified': allNonSignatureFieldsReadOnly};
  },
  expandToShowError: function() {
    this.setState({'minified': false});
  },
  toggleVisibility: function() {
    mixpanel.track('Toggle visibility of to-start header');
    this.setState({'minified': !this.state.minified});
  },
  getAuthenticationMethodText: function() { 
    var sig = this.props.signatory;

    if (sig.standardAuthentication()) return localization.docview.signatory.authenticationStandard;
    if (sig.smsPinAuthentication()) return localization.docview.signatory.authenticationSMSPin;
    if (sig.elegAuthentication()) return localization.docview.signatory.authenticationELeg;

    return "";
  },
  getDeliveryMethodText: function() {
    var sig = this.props.signatory;
    if (sig.emailDelivery()) { return localization.docview.signatory.invitationEmail; }   
    if (sig.padDelivery()) { return localization.docview.signatory.invitationPad; }   
    if (sig.mobileDelivery()) { return localization.docview.signatory.invitationSMS; }   
    if (sig.emailMobileDelivery()) { return localization.docview.signatory.invitationEmailSMS; }   
    if (sig.apiDelivery()) { return localization.docview.signatory.invitationAPI; }   

    return "";
  },  

  renderHeader: function() {
    var signatory = this.props.signatory;
    var imageSource = this.state.minified ? '/img/to-send/arrow-up.png' : '/img/to-send/arrow-down.png';

    return (
      <div className="header-container">
        <h2 className="headline">{signatory.nameOrEmail() || signatory.nameForLists()}</h2>
        <div onClick={this.toggleVisibility} className="arrow-container">
          <div className="background">
            <img className="arrow" src={imageSource} />
          </div>
        </div>
      </div>
    );
  },
  render: function() {
    var signatory = this.props.signatory;
    var fields = signatory.fields();
    var sent = signatory.document().signingInProcess();
    var minified = this.state.minified;
    var containerClasses = "party-container " + (minified ? "minified" : "");
    var svb = this.props.signviewbranding;

    return (
      <div onClick={minified && this.toggleVisibility} className={containerClasses}>

        {this.renderHeader()}

        {fields.map(function(field) {
          return (!field.isSignature() && <ToStartField key={signatory.id + field.name()} field={field} signviewbranding={svb} />);
        })}

        {/* if */ signatory.signs() && 
          <div className="extra-info">
            <div className="auth">
              {localization.docview.signatory.authentication}: {this.getAuthenticationMethodText()}
            </div>
            <div className="delivery">
              {localization.docview.signatory.invitationMethod}: {this.getDeliveryMethodText()}
            </div>
          </div>
        }
      </div>
    );
  }
});

});
