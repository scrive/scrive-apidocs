var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SwedishIdentifyView = require("./swedish/swedishidentifyview");
var SwedishIdentifyModel = require("./swedish/swedishidentifymodel");
var NorwegianIdentifyView = require("./norwegian/norwegianidentifyview");
var NorwegianIdentifyModel = require("./norwegian/norwegianidentifymodel");
var Document = require("../../../js/documents.js").Document;
var $ = require("jquery");

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
      if (this.props.doc.currentSignatory().seBankIDAuthenticationToView()) {
        model = new SwedishIdentifyModel({
          doc: this.props.doc,
          siglinkid: this.props.siglinkid
        });
      } else if (this.props.doc.currentSignatory().noBankIDAuthenticationToView()) {
        model = new NorwegianIdentifyModel({
          doc: this.props.doc,
          siglinkid: this.props.siglinkid
        });
      }
      return {model: model};
    },
    brandLogo: function () {
      var documentId = this.props.doc.documentid();
      var signatoryId = this.props.siglinkid;
      var urlparams = window.brandingdomainid + "/" + documentId + "/" + window.brandinghash;
      return window.cdnbaseurl + "/signview_logo/" + urlparams;
    },

    bankidLogo: function () {
      return window.cdnbaseurl + (this.state.model.isSwedish() ? "/img/bankid2.png" : "/img/bankid-no.png");
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

    render: function () {
      var model = this.state.model;
      var doc = this.props.doc;
      var sig = doc.currentSignatory();
      var author = doc.author();
      var authorName = author.name();
      var personalNumber = sig.personalnumber();

      return (
        <div className="identify-content">
          <div className="identify-logo-box">
            <img className="identify-logo-img" src={this.brandLogo()} />
            <div className="divider-line" />
            <h4 className="identify-logo-text">{localization.esigningpoweredbyscrive}</h4>
          </div>
          <div className="identify-box">
            <div className="identify-box-header">
              {this.verifyIdentityText()}
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
            <div className="identify-box-footer">
              <div className="identify-box-footer-text">
                <div>
                  {localization.header.contact} <b>{authorName || "Empty"}</b>
                </div>
                <div>
                  {localization.identifyDocument} <b>{doc.title()}</b>
                </div>
                <div>
                  {localization.yourIdNumber} <b>{personalNumber || "Empty"}</b>
                </div>
              </div>
              <div className="identify-box-footer-logo">
                <img src={this.bankidLogo()} />
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
