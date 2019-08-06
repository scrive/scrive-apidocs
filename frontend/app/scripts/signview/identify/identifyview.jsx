var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SwedishIdentifyView = require("./swedish/swedishidentifyview");
var SwedishIdentifyModel = require("./swedish/swedishidentifymodel");
var NorwegianIdentifyView = require("./norwegian/norwegianidentifyview");
var NorwegianIdentifyModel = require("./norwegian/norwegianidentifymodel");
var DanishIdentifyView = require("./danish/danishidentifyview");
var DanishIdentifyModel = require("./danish/danishidentifymodel");
var FinnishIdentifyView = require("./finnish/finnishidentifyview");
var FinnishIdentifyModel = require("./finnish/finnishidentifymodel");
var SMSPinIdentifyView = require("./smspin/smspinidentifyview");
var SMSPinIdentifyModel = require("./smspin/smspinidentifymodel");
var VerimiIdentifyView = require("./verimi/verimiidentifyview");
var VerimiIdentifyModel = require("./verimi/verimiidentifymodel");
var Document = require("../../../js/documents.js").Document;
var $ = require("jquery");
var MaskedPersonalNumber = require("./masked_personal_number");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

  module.exports = React.createClass({

    propTypes: {
      doc: React.PropTypes.instanceOf(Document).isRequired,
      siglinkid: React.PropTypes.string.isRequired
    },
    getInitialState: function () {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function (props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps: function (props) {
      var model;
      var args = {
        doc: this.props.doc,
        siglinkid: this.props.siglinkid
      };
      var sig = args.doc.currentSignatory();
      if (args.doc.closed()) {
        if (sig.seBankIDAuthenticationToViewArchived()) {
          model = new SwedishIdentifyModel(args);
        } else if (sig.noBankIDAuthenticationToViewArchived()) {
          model = new NorwegianIdentifyModel(args);
        } else if (sig.dkNemIDAuthenticationToViewArchived()) {
          model = new DanishIdentifyModel(args);
        } else if (sig.fiTupasAuthenticationToViewArchived()) {
          model = new FinnishIdentifyModel(args);
        } else if (sig.smsPinAuthenticationToViewArchived()) {
          model = new SMSPinIdentifyModel(args);
        }  else if (sig.verimiAuthenticationToViewArchived()) {
          model = new VerimiIdentifyModel(args);
        }
      } else {
        if (sig.seBankIDAuthenticationToView()) {
          model = new SwedishIdentifyModel(args);
        } else if (sig.noBankIDAuthenticationToView()) {
          model = new NorwegianIdentifyModel(args);
        } else if (sig.dkNemIDAuthenticationToView()) {
          model = new DanishIdentifyModel(args);
        } else if (sig.fiTupasAuthenticationToView()) {
          model = new FinnishIdentifyModel(args);
        } else if (sig.smsPinAuthenticationToView()) {
          model = new SMSPinIdentifyModel(args);
        } else if (sig.verimiAuthenticationToView()) {
          model = new VerimiIdentifyModel(args);
        }
      }
      return {model: model};
    },
    brandLogo: function () {
      var documentId = this.props.doc.documentid();
      var signatoryId = this.props.siglinkid;
      var urlparams = window.brandingdomainid + "/" + documentId + "/" + window.brandinghash;
      return window.cdnbaseurl + "/signview_logo/" + urlparams;
    },
    verifyIdentityText: function () {
      var model = this.state.model;
      var doc = this.props.doc;
      var sig = doc.currentSignatory();
      if (sig.name()) {
        var textWrapper =  $("<span>" + localization.verifyIdentityWithName + "</span>");
        textWrapper.find(".name-of-signatory").text(sig.name());
        return textWrapper.text();
      } else {
        return localization.verifyIdentityWithoutName;
      }
    },

    localizationStringForSsnInfoText: function () {
      var model = this.state.model;
      if (model.isDanish()) {
        return localization.eID.infoText.nemId;
      } else {
        return localization.yourIdNumber;
      }
    },

    render: function () {
      var model = this.state.model;
      var doc = this.props.doc;
      var sig = doc.currentSignatory();
      var author = doc.author();
      var authorName = author.name();
      var personalNumber = sig.personalnumber();
      var mobileNumber = sig.mobile();
      var email = sig.email();

      return (
        <div className="identify-content">
          <div className="identify-logo-box">
            <img className="identify-logo-img" src={this.brandLogo()} />
            <div className="divider-line" />
            <h4 className="identify-logo-text">{localization.esigningpoweredbyscrive}</h4>
          </div>
          <div className="identify-box">
            <div className="identify-box-header">
              { /* if */ sig.name() &&
                <HtmlTextWithSubstitution
                  secureText={localization.verifyIdentityWithName}
                  subs={{".name-of-signatory": sig.name()}}
                />
              }
              { /* else */ !sig.name() &&
                <span>{localization.verifyIdentityWithoutName}</span>
              }
            </div>
            { /* if */ model.isSwedish() &&
              <div className="identify-box-content">
                <SwedishIdentifyView
                  ref="identify"
                  model={model}
                />
              </div>
            }
            { /* else if */ model.isNorwegian() &&
              <NorwegianIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ model.isDanish() &&
              <DanishIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ model.isFinnish() &&
              <FinnishIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ model.isSMSPin() &&
              <SMSPinIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ model.isVerimi() &&
              <VerimiIdentifyView
                ref="identify"
                model={model}
              />
            }
            <div className="identify-box-footer">
              <div className="identify-box-footer-text">
                <div>
                  {localization.header.contact} <b>{authorName || "Empty"}</b>
                </div>
                <div>
                  {localization.identifyDocument} <b>{doc.title()}</b>
                </div>
                { /* if */ (model.isSwedish() || model.isNorwegian() || model.isDanish() || model.isFinnish()) &&
                  <div>
                    {this.localizationStringForSsnInfoText()} <MaskedPersonalNumber
                      number={personalNumber}
                      placeholder="Empty"
                      isNorwegian={model.isNorwegian()}
                      isDanish={model.isDanish()}
                      isFinnish={model.isFinnish()}
                    />
                  </div>
                }
                { /* if */ (model.isSMSPin()) &&
                  <div>
                    {localization.yourMobileNumber} <b>{mobileNumber}</b>
                  </div>
                }
                { /* if */ (model.isVerimi()) &&
                  <div>
                    {localization.yourEmail} <b>{email}</b>
                  </div>
                }
              </div>
              <div className="identify-box-footer-logo">
                { /* if */ model.isSwedish() &&
                  <img src={window.cdnbaseurl + "/img/mobilebankid.png"} className="identify-box-footer-first-logo"/>
                }
                { /* if */ model.isSwedish() &&
                  <img src={window.cdnbaseurl + "/img/bankid2.png"} />
                }
                { /* if */ model.isNorwegian() &&
                  <img src={window.cdnbaseurl + "/img/bankid-no.png"} />
                }
                { /* if */ model.isDanish() &&
                  <img src={window.cdnbaseurl + "/img/nemid-dk.png"} />
                }
                { /* if */ model.isFinnish() &&
                  <img src={window.cdnbaseurl + "/img/tupas-fi.png"} />
                }
                { /* if */ model.isVerimi() &&
                  <img src={window.cdnbaseurl + "/img/verimi.svg"} />
                }
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
