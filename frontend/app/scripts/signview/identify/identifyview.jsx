var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SwedishIdentifyView = require("./swedish/swedishidentifyview");
var SwedishIdentifyModel = require("./swedish/swedishidentifymodel");
var SwedishEIDHubIdentifyView = require("./swedish-eidhub/swedishidentifyview");
var SwedishEIDHubIdentifyModel = require("./swedish-eidhub/swedishidentifymodel");
var NorwegianEIDHubIdentifyView = require("./norwegian-eidhub/norwegianidentifyview");
var NorwegianEIDHubIdentifyModel = require("./norwegian-eidhub/norwegianidentifymodel");
var DanishEIDHubIdentifyView = require("./danish-eidhub/danishidentifyview");
var DanishEIDHubIdentifyModel = require("./danish-eidhub/danishidentifymodel");
var FinnishIdentifyView = require("./finnish/finnishidentifyview");
var FinnishIdentifyModel = require("./finnish/finnishidentifymodel");
var FinnishEIDHubIdentifyView = require("./finnish-eidhub/finnishidentifyview");
var FinnishEIDHubIdentifyModel = require("./finnish-eidhub/finnishidentifymodel");
var SMSPinIdentifyView = require("./smspin/smspinidentifyview");
var SMSPinIdentifyModel = require("./smspin/smspinidentifymodel");
var VerimiIdentifyView = require("./verimi/verimiidentifyview");
var VerimiIdentifyModel = require("./verimi/verimiidentifymodel");
var IDINIdentifyView = require("./idin/idinidentifyview");
var IDINIdentifyModel = require("./idin/idinidentifymodel");
var Document = require("../../../js/documents.js").Document;
var $ = require("jquery");
var MaskedPersonalNumber = require("./masked_personal_number");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

  module.exports = React.createClass({

    propTypes: {
      doc: React.PropTypes.instanceOf(Document).isRequired,
      siglinkid: React.PropTypes.string.isRequired,
      useEIDHubForSEBankIDView: React.PropTypes.bool.isRequired,
      useEIDHubForFITupasView: React.PropTypes.bool.isRequired
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
          if (this.props.useEIDHubForSEBankIDView) {
            model = new SwedishEIDHubIdentifyModel(args);
          } else {
            model = new SwedishIdentifyModel(args);
          }
        } else if (sig.noBankIDAuthenticationToViewArchived()) {
          model = new NorwegianEIDHubIdentifyModel(args);
        } else if (sig.dkNemIDCPRAuthenticationToViewArchived()
                   || sig.dkNemIDPIDAuthenticationToViewArchived()
                   || sig.dkNemIDCVRAuthenticationToViewArchived()) {
          model = new DanishEIDHubIdentifyModel(args);
        } else if (sig.fiTupasAuthenticationToViewArchived()) {
          if (this.props.useEIDHubForFITupasView) {
            model = new FinnishEIDHubIdentifyModel(args);
          } else {
            model = new FinnishIdentifyModel(args);
          }
        } else if (sig.smsPinAuthenticationToViewArchived()) {
          model = new SMSPinIdentifyModel(args);
        } else if (sig.verimiAuthenticationToViewArchived()) {
          model = new VerimiIdentifyModel(args);
        } else if (sig.idinAuthenticationToViewArchived()) {
          model = new IDINIdentifyModel(args);
        }
      } else {
        if (sig.seBankIDAuthenticationToView()) {
          if (this.props.useEIDHubForSEBankIDView) {
            model = new SwedishEIDHubIdentifyModel(args);
          } else {
            model = new SwedishIdentifyModel(args);
          }
        } else if (sig.noBankIDAuthenticationToView()) {
          model = new NorwegianEIDHubIdentifyModel(args);
        } else if (sig.dkNemIDCPRAuthenticationToView()
                   || sig.dkNemIDPIDAuthenticationToView()
                   || sig.dkNemIDCVRAuthenticationToView()) {
          model = new DanishEIDHubIdentifyModel(args);
        } else if (sig.fiTupasAuthenticationToView()) {
          if (this.props.useEIDHubForFITupasView) {
            model = new FinnishEIDHubIdentifyModel(args);
          } else {
            model = new FinnishIdentifyModel(args);
          }
        } else if (sig.smsPinAuthenticationToView()) {
          model = new SMSPinIdentifyModel(args);
        } else if (sig.verimiAuthenticationToView()) {
          model = new VerimiIdentifyModel(args);
        } else if (sig.idinAuthenticationToView()) {
          model = new IDINIdentifyModel(args);
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
      if (model.isDanishPersonal()) {
        return localization.eID.infoText.cpr;
      } else if (model.isDanishEmployee()) {
        return localization.eID.infoText.cvr;
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
            { /* if */ model.isSwedish() && this.props.useEIDHubForSEBankIDView &&
              <div className="identify-box-content">
                <SwedishEIDHubIdentifyView
                  ref="identify"
                  model={model}
                />
              </div>
            }
            { /* if */ model.isSwedish() && !this.props.useEIDHubForSEBankIDView &&
              <div className="identify-box-content">
                <SwedishIdentifyView
                  ref="identify"
                  model={model}
                />
              </div>
            }
            { /* else if */ model.isNorwegian() &&
              <NorwegianEIDHubIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ (model.isDanishPersonal() || model.isDanishEmployee()) &&
              <DanishEIDHubIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ model.isFinnish() && !this.props.useEIDHubForFITupasView &&
              <FinnishIdentifyView
                ref="identify"
                model={model}
              />
            }
            { /* else if */ model.isFinnish() && this.props.useEIDHubForFITupasView &&
              <FinnishEIDHubIdentifyView
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
            { /* else if */ model.isIDIN() &&
              <IDINIdentifyView
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
                { /* if */ (model.isSwedish()
                            || model.isNorwegian()
                            || model.isDanishPersonal()
                            || model.isDanishEmployee()
                            || model.isFinnish()) &&
                  <div>
                    {this.localizationStringForSsnInfoText()} <MaskedPersonalNumber
                      number={personalNumber}
                      placeholder="Empty"
                      isNorwegian={model.isNorwegian()}
                      isDanishPersonal={model.isDanishPersonal()}
                      isDanishEmployee={model.isDanishEmployee()}
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
                { /* if */ (model.isIDIN()) &&
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
                { /* if */ (model.isDanishPersonal() || model.isDanishEmployee()) &&
                  <img src={window.cdnbaseurl + "/img/nemid-dk.png"} />
                }
                { /* if */ model.isFinnish() &&
                  <img src={window.cdnbaseurl + "/img/tupas-fi.png"} />
                }
                { /* if */ model.isVerimi() &&
                  <img src={window.cdnbaseurl + "/img/verimi.svg"} />
                }
                { /* if */ model.isIDIN() &&
                  <img src={window.cdnbaseurl + "/img/iDIN.png"} />
                }
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
