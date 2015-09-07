/** @jsx React.DOM */

define(["React", "common/button", "common/backbone_mixin", "Backbone",
  "common/language_service", "legacy_code"],
  function (React, Button, BackboneMixin, Backbone, LanguageService) {

  var expose = {};

  var DocumentViewSignatoryModel = Backbone.Model.extend({
    initialize: function (args) {
      var self = this;
      this.listenTo(args.signatory, "change", function () {self.trigger("change");});
    },

    document:function () {
      return this.signatory().document();
    },

    signatory: function () {
      return this.get("signatory");
    },

    status: function () {
      return this.signatory().status();
    },

    signatorySummary: function () {
      var signatory = this.signatory();
      var document = this.document();
      if (signatory.signdate() != undefined) {
        return localization.signatoryMessage.signed;
      } else if (document.timedout() || document.canceled() || document.rejected()) {
        return localization.docsignview.unavailableForSign;
      } else if (signatory.rejecteddate() != undefined) {
        return localization.signatoryMessage.rejected;
      } else if (signatory.status() == "opened") {
        return localization.signatoryMessage.seen;
      } else if (signatory.status() == "sent" && signatory.reachedBySignorder()) {
        return localization.signatoryMessage.other;
      } else if (signatory.status() == "sent") {
        return localization.signatoryMessage.waiting;
      } else if (localization.signatoryMessage[signatory.status()] != undefined) {
        return localization.signatoryMessage[signatory.status()];
      } else {
        return localization.signatoryMessage.other;
      }
    },

    hasAnyDetails: function () {
     var signatory = this.signatory();
     return signatory.company()
       || signatory.email()
       || signatory.mobile()
       || signatory.companynumber()
       || signatory.personalnumber();
    }
  });

  var DocumentViewSignatoryForListView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    propTypes: {
      model: React.PropTypes.object
    },

    onSelect: function () {
      this.props.onSelect();
    },

    render: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      var divClass = React.addons.classSet({
        "sig": true,
        "first": this.props.first,
        "last": this.props.last,
        "active": this.props.active
      });

      return (
        <div onClick={this.onSelect} className={divClass}>
          {/* if */ (this.props.active) &&
            <div className="arrow"/>
          }
          <div className="name">
            {signatory.nameOrEmailOrMobile()}{"\u00A0"}
          </div>
          <div className="line">
            <div className="middle">
              <div className={"icon status " + model.status()}> </div>
            </div>
            <div className="middle">
              <div className={"statustext " + model.status()}>
                  {model.signatorySummary()}
              </div>
            </div>
            <div className="middle details">
            </div>
          </div>
        </div>
      );
    }
  });

  var DocumentViewSignatoryView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model];
    },

    propTypes: {
      model: React.PropTypes.object
    },

    goToSignView: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      LocalStorage.set("backlink", "target", "to-sign");
      mixpanel.track("Accept", {"Signatory index":signatory.signIndex(), "Accept": "give for signing"});
      signatory.giveForPadSigning().send();
    },

    getDeliveryMethod: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      if (signatory.emailDelivery()) {
        return localization.docview.signatory.invitationEmail;
      } else if (signatory.padDelivery()) {
        return localization.docview.signatory.invitationPad;
      } else if (signatory.mobileDelivery()) {
        return localization.docview.signatory.invitationSMS;
      } else if (signatory.emailMobileDelivery()) {
        return localization.docview.signatory.invitationEmailSMS;
      } else if (signatory.apiDelivery()) {
        return localization.docview.signatory.invitationAPI;
      } else if (signatory.noneDelivery()) {
        return localization.docview.signatory.invitationNone;
      }
    },

    getRole: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      if (signatory.signs()) {
        return localization.docview.signatory.roleSignatory;
      } else {
        return localization.docview.signatory.roleViewer;
      }
    },

    getAuthenticationToViewMethodText: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      if (signatory.standardAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewStandard;
      } else if (signatory.seBankIDAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewSEBankID;
      } else if (signatory.noBankIDAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewNOBankID;
      }
    },

    getAuthenticationToSignMethodText: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      if (signatory.standardAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignStandard;
      } else if (signatory.smsPinAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignSMSPin;
      } else if (signatory.seBankIDAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignSEBankID;
      }
    },

    getConfirmationMethod: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      if (signatory.emailConfirmationDelivery()) {
        return localization.docview.signatory.confirmationEmail;
      } else if (signatory.mobileConfirmationDelivery()) {
        return localization.docview.signatory.confirmationSMS;
      } else if (signatory.emailMobileConfirmationDelivery()) {
        return localization.docview.signatory.confirmationEmailSMS;
      } else if (signatory.noneConfirmationDelivery()) {
        return localization.docview.signatory.confirmationNone;
      }
    },

    render: function () {
      var model = this.props.model;
      var signatory = model.signatory();

      return (
        <div className="grey-box">
          <div className="titleinfo spacing">
            <div className="name">
              {signatory.nameOrEmailOrMobile()}{"\u00A0"}
            </div>
          </div>
          <div className={model.hasAnyDetails() ? "inner fields" : ""} >
            {/* if */ signatory.company() &&
              <div className="fieldrow">
                <span className="company field" title={signatory.company()}>
                  {localization.company}: {signatory.company()}
                </span>
              </div>
            }
            {/* if */ signatory.email() &&
              <div className="fieldrow">
                <span className="email field" display={false} title={signatory.email()}>
                  {localization.email}: {signatory.email()}
                </span>
              </div>
            }
            {/* if */ signatory.mobile() &&
              <div className="fieldrow">
                <span className="mobile field" title={signatory.mobile()}>
                  {localization.phone}: {signatory.mobile()}
                </span>
              </div>
            }
            {/* if */ signatory.companynumber() &&
              <div className="fieldrow">
                <span className="orgnum field" title={signatory.companynumber()}>
                  {localization.docsignview.companyNumberLabel}:
                  {signatory.companynumber().trim() || localization.docsignview.notEntered}
                </span>
              </div>
            }
            {/* if */ signatory.personalnumber() &&
              <div className="fieldrow">
                <span className="persnum field" title={signatory.personalnumber()}>
                  {localization.docsignview.personalNumberLabel}:
                  {signatory.personalnumber().trim() || localization.docsignview.notEntered}
                </span>
              </div>
            }
          </div>
          <div className="statusbox">
            <div className="spacing butt" >
              <span className={"icon status " + model.status()}></span>
              <span className={"status statustext " + model.status()}>
                {model.signatorySummary()}
              </span>
            </div>
          </div>
        </div>
      );
    }
  });

  var DocumentViewSignatoryForList = React.createClass({
    propTypes: {
      signatory: React.PropTypes.object,
      first: React.PropTypes.bool.isRequired,
      last: React.PropTypes.bool.isRequired,
      active: React.PropTypes.bool.isRequired,
      onSelect: React.PropTypes.func
    },

    getInitialState: function () {
      return this.stateFromProps(this.props);
    },

    componentWillReceiveProps: function (props) {
      this.setState(this.stateFromProps(props));
    },

    stateFromProps: function (props) {
      var model = new DocumentViewSignatoryModel({
        signatory: props.signatory
      });
      return {model: model};
    },

    render: function () {
      return (
        <DocumentViewSignatoryForListView
          model={this.state.model}
          first={this.props.first}
          last={this.props.last}
          active={this.props.active}
          onSelect={this.props.onSelect}
        />
      );
    }
  });

  var DocumentViewSignatory = React.createClass({
    propTypes: {
      signatory: React.PropTypes.object
    },

    getInitialState: function () {
      return this.stateFromProps(this.props);
    },

    componentWillReceiveProps: function (props) {
      this.setState(this.stateFromProps(props));
    },

    stateFromProps: function (props) {
      var model = new DocumentViewSignatoryModel({
        signatory: props.signatory
      });
      return {model: model};
    },

    render: function () {
      return (
        <DocumentViewSignatoryView model={this.state.model}/>
      );
    }
  });

  expose.DocumentViewSignatoryForList = DocumentViewSignatoryForList;
  expose.DocumentViewSignatory = DocumentViewSignatory;

  return expose;
});
