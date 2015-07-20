define(["legacy_code",  "React", "signview/identify/identifymodel",
        "common/backbone_mixin", "common/button", "common/checkbox", "signview/identify/swedishidentify",
        "signview/identify/swedishprocessing", "signview/identify/swedishproblem",
        "signview/identify/norwegianidentify", "signview/identify/norwegianprocessing",
        "signview/identify/norwegianproblem"],
  function (legacy_code, React, IdentifyModel, BackboneMixin, Button,
            Checkbox, SwedishIdentify, SwedishProcessing, SwedishProblem,
            NorwegianIdentify, NorwegianProcessing, NorwegianProblem) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

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
      var model = new IdentifyModel({
        doc: this.props.doc,
        siglinkid: this.props.siglinkid
      });
      return {model: model};
    },

    getBackboneModels: function () {
      return [this.state.model];
    },

    brandLogo: function () {
      var documentId = this.state.model.doc().documentid();
      var signatoryId = this.state.model.siglinkid();
      return "/signview_logo/" + documentId + "/" + signatoryId + "/" + window.brandinghash;
    },

    bankidLogo: function () {
      return this.state.model.isSwedish() ? "/img/bankid2.png" : "/img/bankid-no.png";
    },
    verifyIdentityText: function () {
      var model = this.state.model;
      var doc = model.doc();
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
      var doc = model.doc();
      var sig = doc.currentSignatory();
      var author = doc.author();
      var authorName = author.name();
      var personalNumber = sig.personalnumber();

      var Content = {};
      if (model.isSwedish()) {
        Content.Identify = SwedishIdentify;
        Content.Processing = SwedishProcessing;
        Content.Problem = SwedishProblem;
      }
      if (model.isNorwegian()) {
        Content.Identify = NorwegianIdentify;
        Content.Processing = NorwegianProcessing;
        Content.Problem = NorwegianProblem;
      }

      var handleIdentify = model.isSwedish() ? model.identifySwedish : model.identifyNorwegian;
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
            <div className="identify-box-content">
              { /* if */ model.isIdentify() &&
                <Content.Identify
                  ref="identify"
                  personalNumber={personalNumber}
                  phoneNumber={sig.mobile()}
                  onIdentify={handleIdentify}
                  onSetThisDevice={function (v) {model.setThisDevice(v);}}
                  thisDevice={model.thisDevice()}
                />
              }
              { /* else if */ model.isProcessing() &&
                <Content.Processing
                  ref="processing"
                  transaction={model.transaction()}
                  onCancel={model.cancel}
                  statusText={model.statusText()}
                />
              }
              { /* else if */  model.isProblem() &&
                <Content.Problem
                  ref="problem"
                  onBack={model.back}
                  statusText={model.statusText()}
                />
              }
            </div>
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
});
