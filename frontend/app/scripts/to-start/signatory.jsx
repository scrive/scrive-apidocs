var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Track = require("../common/track");
var ToStartField = require("./field");
var _ = require("underscore");


module.exports = React.createClass({
  propTypes: {
    signatory: React.PropTypes.object
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
    Track.track('Toggle visibility of to-start header');
    this.setState({'minified': !this.state.minified});
  },
  getAuthenticationMethodText: function() {
    var sig = this.props.signatory;

    if (sig.standardAuthenticationToSign()) return localization.docview.signatory.authenticationToSignStandard;
    if (sig.smsPinAuthenticationToSign()) return localization.docview.signatory.authenticationToSignSMSPin;
    if (sig.seBankIDAuthenticationToSign()) return localization.docview.signatory.authenticationToSignSEBankID;
    if (sig.noBankIDAuthenticationToSign()) return localization.docview.signatory.authenticationToSignNOBankID;
    if (sig.dkNemIDCPRAuthenticationToSign()) return localization.docview.signatory.authenticationToSignDKNemIDCPR;
    if (sig.dkNemIDPIDAuthenticationToSign()) return localization.docview.signatory.authenticationToSignDKNemIDPID;
    if (sig.dkNemIDCVRAuthenticationToSign()) return localization.docview.signatory.authenticationToSignDKNemIDCVR;

    return "";
  },
  getDeliveryMethodText: function() {
    var sig = this.props.signatory;
    if (sig.emailDelivery()) { return localization.docview.signatory.invitationEmail; }
    if (sig.padDelivery()) { return localization.docview.signatory.invitationPad; }
    if (sig.mobileDelivery()) { return localization.docview.signatory.invitationSMS; }
    if (sig.emailMobileDelivery()) { return localization.docview.signatory.invitationEmailSMS; }
    if (sig.apiDelivery()) { return localization.docview.signatory.invitationLink; }

    return "";
  },
  focusOfFirstField: function() {
    var field =_.find(this.props.signatory.fields(), function(f) {
      return !f.isSignature() && !f.isCheckbox() && !f.isAuthorUnchangeableField();
    });
    if (field && this.refs["field-" + field.cid] != undefined) {
      this.refs["field-" + field.cid].focus();
    }
  },
  render: function() {
    var signatory = this.props.signatory;
    var fields = signatory.fields();
    var sent = signatory.document().pending();
    var minified = this.state.minified;
    var containerClasses = "party-container " + (minified ? "minified" : "");
    var imageSource = window.cdnbaseurl + (this.state.minified ? '/img/to-send/arrow-up.png' : '/img/to-send/arrow-down.png');

    return (
      <div onClick={minified && this.toggleVisibility} className={containerClasses}>

        <div className="header-container">
          <h2 className="headline">{signatory.nameOrEmail() || signatory.nameForLists()}</h2>
          <div onClick={this.toggleVisibility} className="arrow-container">
            <div className="background">
              <img className="arrow" src={imageSource} />
            </div>
          </div>
        </div>

        {fields.map(function(field) {
          if (!field.isSignature()) {
            return (
              <ToStartField
                key={"field-" + field.cid}
                ref={"field-" + field.cid}
                field={field}
              />
            );
          }
        })}

        {/* if */ signatory.signs() &&
          <div className="extra-info">
            <div className="auth">
              {localization.docview.signatory.authenticationToSign}: {this.getAuthenticationMethodText()}
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
