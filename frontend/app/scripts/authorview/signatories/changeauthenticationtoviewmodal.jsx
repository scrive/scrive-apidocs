var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var SSNForSEBankIDValidation = require("../../../js/validation.js").SSNForSEBankIDValidation;
var SSNForNOBankIDValidation = require("../../../js/validation.js").SSNForNOBankIDValidation;
var SSNForDKNemIDValidation = require("../../../js/validation.js").SSNForDKNemIDValidation;
var CVRForDKNemIDValidation = require("../../../js/validation.js").CVRForDKNemIDValidation;
var SSNForFITupasValidation = require("../../../js/validation.js").SSNForFITupasValidation;
var PhoneValidationNO = require("../../../js/validation.js").PhoneValidationNO;
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;
var EmptyValidation = require("../../../js/validation.js").EmptyValidation;
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var $ = require("jquery");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Track = require("../../common/track");
var classNames = require("classnames");
var Subscription = require("../../account/subscription");

var Modal = require("../../common/modal");

  var ChangeAuthenticationToViewModalModel = Backbone.Model.extend({
    initialize: function (args) {
      this.setAuthenticationMethod(this.signatory().authenticationToView());
    },

    document: function () {
      return this.signatory().document();
    },

    signatory: function () {
      return this.get("signatory");
    },

    authenticationMethod: function () {
      return this.get("authenticationMethod");
    },

    setAuthenticationMethod: function (newMethod) {
      var previousMethod = this.authenticationMethod();
      this.set({authenticationMethod: newMethod});
      if (previousMethod != newMethod) {
        var signatory = this.signatory();
        this.setPersonalNumber("");
        this.setMobileNumber("");
        if (this.isAuthenticationSEBankID()) {
          this.setPersonalNumber(signatory.personalnumber());
        } else if (this.isAuthenticationNOBankID()) {
          this.setPersonalNumber(signatory.personalnumber());
          this.setMobileNumber(signatory.mobile());
        } else if (this.isAuthenticationDKNemIDCPR()
                   || this.isAuthenticationDKNemIDPID()
                   || this.isAuthenticationLegacyDKNemID()) {
          this.setPersonalNumber(signatory.personalnumber());
        } else if (this.isAuthenticationDKNemIDCVR()) {
          this.setPersonalNumber(signatory.personalnumber());
        } else if (this.isAuthenticationFITupas()) {
          this.setPersonalNumber(signatory.personalnumber());
        } else if (this.isAuthenticationSMSPin()) {
          this.setMobileNumber(signatory.mobile());
        }
      }
    },

    standardAuthenticationValue: function () {
      return "standard";
    },

    isAuthenticationStandard: function () {
      return this.authenticationMethod() == this.standardAuthenticationValue();
    },

    SEBankIDAuthenticationValue: function () {
      return "se_bankid";
    },

    isAuthenticationSEBankID: function () {
      return this.authenticationMethod() == this.SEBankIDAuthenticationValue();
    },

    NOBankIDAuthenticationValue: function () {
      return "no_bankid";
    },

    isAuthenticationNOBankID: function () {
      return this.authenticationMethod() == this.NOBankIDAuthenticationValue();
    },

    DKNemIDCPRAuthenticationValue: function () {
      return "dk_nemid_cpr";
    },

    isAuthenticationDKNemIDCPR: function () {
      return this.authenticationMethod() == this.DKNemIDCPRAuthenticationValue();
    },

    LegacyDKNemIDAuthenticationValue: function () {
      return "dk_nemid";
    },

    isAuthenticationLegacyDKNemID: function () {
      return this.authenticationMethod() == this.LegacyDKNemIDAuthenticationValue();
    },

    DKNemIDPIDAuthenticationValue: function () {
      return "dk_nemid_pid";
    },

    isAuthenticationDKNemIDPID: function () {
      return this.authenticationMethod() == this.DKNemIDPIDAuthenticationValue();
    },

    DKNemIDCVRAuthenticationValue: function () {
      return "dk_nemid_cvr";
    },

    isAuthenticationDKNemIDCVR: function () {
      return this.authenticationMethod() == this.DKNemIDCVRAuthenticationValue();
    },

    FITupasAuthenticationValue: function () {
      return "fi_tupas";
    },

    isAuthenticationFITupas: function () {
      return this.authenticationMethod() == this.FITupasAuthenticationValue();
    },

    SMSPinAuthenticationValue: function () {
      return "sms_pin";
    },

    isAuthenticationSMSPin: function () {
      return this.authenticationMethod() == this.SMSPinAuthenticationValue();
    },

    VerimiAuthenticationValue: function () {
      return "verimi";
    },

    isAuthenticationVerimi: function () {
      return this.authenticationMethod() == this.VerimiAuthenticationValue();
    },

    idinAuthenticationValue: function () {
      return "nl_idin";
    },

    isAuthenticationIDIN: function () {
      return this.authenticationMethod() == this.idinAuthenticationValue();
    },

    personalNumber: function () {
      return this.get("personalNumber");
    },

    setPersonalNumber: function (number) {
      this.set({personalNumber: number});
    },

    isPersonalNumberValid: function () {
      var pn = this.personalNumber();
      if (this.isAuthenticationSEBankID()) {
        return new SSNForSEBankIDValidation().validateData(pn);
      } else if (this.isAuthenticationNOBankID()) {
        return new SSNForNOBankIDValidation().validateData(pn);
      } else if (this.isAuthenticationDKNemIDCPR() || this.isAuthenticationDKNemIDPID()) {
        return new SSNForDKNemIDValidation().validateData(pn);
      } else if (this.isAuthenticationDKNemIDCVR()) {
        return new CVRForDKNemIDValidation().validateData(pn);
      } else if (this.isAuthenticationFITupas()) {
        return new EmptyValidation().or(new SSNForFITupasValidation()).validateData(pn);
      }
      return true;
    },

    mobileNumber: function () {
      return this.get("mobileNumber");
    },

    setMobileNumber: function (number) {
      this.set({mobileNumber: number});
    },

    isMobileNumberValid: function () {
      var m = this.mobileNumber();
      if (this.isAuthenticationNOBankID()) {
        return (
          new PhoneValidationNO().validateData(m) ||
          new EmptyValidation().validateData(m)
        );
      } else if (this.isAuthenticationSMSPin()) {
        return (
          new PhoneValidation().validateData(m)
        );
      }
      return true;
    },

    isValid: function () {
      return this.isPersonalNumberValid() && this.isMobileNumberValid();
    },

    getInvalidFlashMessageText: function () {
      var text = "";
      if (this.isAuthenticationSEBankID() && !this.isPersonalNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidSESSN;
      } else if (this.isAuthenticationNOBankID()) {
        if (!this.isPersonalNumberValid() && !this.isMobileNumberValid()) {
          text = localization.docview.changeAuthenticationToView.flashMessageInvalidNOSSNAndPhone;
        } else if (!this.isPersonalNumberValid()) {
          text = localization.docview.changeAuthenticationToView.flashMessageInvalidNOSSN;
        } else if (!this.isMobileNumberValid()) {
          text = localization.docview.changeAuthenticationToView.flashMessageInvalidNOPhone;
        }
      } else if ((this.isAuthenticationDKNemIDCPR() || this.isAuthenticationDKNemIDPID())
                 && !this.isPersonalNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidDKSSN;
      } else if (this.isAuthenticationDKNemIDCVR() && !this.isPersonalNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidDKCVR;
      } else if (this.isAuthenticationFITupas() && !this.isPersonalNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidFISSN;
      } else if (this.isAuthenticationSMSPin() && !this.isMobileNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidNOPhone;
      }
      return text;
    }
  });

  var ChangeAuthenticationToViewModalView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels: function () {
      return [this.props.model];
    },

    propTypes: {
      model: React.PropTypes.object
    },

    setAuthenticationMethod: function (v) {
      var model = this.props.model;
      model.setAuthenticationMethod(v);
    },

    getAuthenticationMethodNameText: function () {
      var model = this.props.model;
      if (model.isAuthenticationStandard()) {
        return localization.docview.signatory.authenticationToViewStandard;
      } else if (model.isAuthenticationSEBankID()) {
        return localization.docview.signatory.authenticationToViewSEBankID;
      } else if (model.isAuthenticationNOBankID()) {
        return localization.docview.signatory.authenticationToViewNOBankID;
      } else if (model.isAuthenticationDKNemIDCPR() || model.isAuthenticationLegacyDKNemID()) {
        return localization.docview.signatory.authenticationToViewDKNemIDCPR;
      } else if (model.isAuthenticationDKNemIDPID()) {
        return localization.docview.signatory.authenticationToViewDKNemIDPID;
      } else if (model.isAuthenticationDKNemIDCVR()) {
        return localization.docview.signatory.authenticationToViewDKNemIDCVR;
      } else if (model.isAuthenticationFITupas()) {
        return localization.docview.signatory.authenticationToViewFITupas;
      } else if (model.isAuthenticationSMSPin()) {
        return localization.docview.signatory.authenticationToViewSMSPin;
      }
    },

    getAuthenticationMethodOptions: function () {
      var ff = Subscription.currentSubscription().currentUserFeatures();
      var model = this.props.model;
      var sig = model.signatory();
      var options = [];

      var isAvailable = function (auth) {

        // Changing email is tricky - I prefer just not to allow that
        // and block switching to Verimi if email is not valid.
        if (auth == "verimi" && !new EmailValidation().validateData(sig.email())) {
          return false;
        }

        return sig.authenticationMethodsCanMix(auth,
                                               sig.authenticationToSign(),
                                               sig.authenticationToViewArchived());
      };

      var standard = {
        name: localization.docview.signatory.authenticationToViewStandard,
        selected: model.isAuthenticationStandard(),
        value: model.standardAuthenticationValue()
      };
      if (ff.canUseStandardAuthenticationToView() && isAvailable(standard.value)) {
        options.push(standard);
      }

      var seBankid = {
        name: localization.docview.signatory.authenticationToViewSEBankID,
        selected: model.isAuthenticationSEBankID(),
        value: model.SEBankIDAuthenticationValue()
      };
      if (ff.canUseSEAuthenticationToView() && isAvailable(seBankid.value)) {
        options.push(seBankid);
      }

      var noBankid = {
        name: localization.docview.signatory.authenticationToViewNOBankID,
        selected: model.isAuthenticationNOBankID(),
        value: model.NOBankIDAuthenticationValue()
      };
      if (ff.canUseNOAuthenticationToView() && isAvailable(noBankid.value)) {
        options.push(noBankid);
      }

      var dkNemidCPR = {
        name: localization.docview.signatory.authenticationToViewDKNemIDCPR,
        selected: model.isAuthenticationDKNemIDCPR(),
        value: model.DKNemIDCPRAuthenticationValue()
      };
      if (ff.canUseDKCPRAuthenticationToView() && isAvailable(dkNemidCPR.value)) {
        options.push(dkNemidCPR);
      }

      var dkNemidPID = {
        name: localization.docview.signatory.authenticationToViewDKNemIDPID,
        selected: model.isAuthenticationDKNemIDPID(),
        value: model.DKNemIDPIDAuthenticationValue()
      };
      if (ff.canUseDKPIDAuthenticationToView() && isAvailable(dkNemidPID.value)) {
        options.push(dkNemidPID);
      }

      var dkNemidCVR = {
        name: localization.docview.signatory.authenticationToViewDKNemIDCVR,
        selected: model.isAuthenticationDKNemIDCVR(),
        value: model.DKNemIDCVRAuthenticationValue()
      };
      if (ff.canUseDKCVRAuthenticationToView() && isAvailable(dkNemidCVR.value)) {
        options.push(dkNemidCVR);
      }

      var fiTupas = {
        name: localization.docview.signatory.authenticationToViewFITupas,
        selected: model.isAuthenticationFITupas(),
        value: model.FITupasAuthenticationValue()
      };
      if (ff.canUseFIAuthenticationToView() && isAvailable(fiTupas.value)) {
        options.push(fiTupas);
      }

      var smsPin = {
        name: localization.docview.signatory.authenticationToViewSMSPin,
        selected: model.isAuthenticationSMSPin(),
        value: model.SMSPinAuthenticationValue()
      };
      if (ff.canUseSMSPinAuthenticationToView() && isAvailable(smsPin.value)) {
        options.push(smsPin);
      }

      var verimi = {
        name: localization.docview.signatory.authenticationToViewVerimi,
        selected: model.isAuthenticationVerimi(),
        value: model.VerimiAuthenticationValue()
      };
      if (ff.canUseVerimiAuthenticationToView() && isAvailable(verimi.value)) {
        options.push(verimi);
      }

      var idin = {
        name: localization.docview.signatory.authenticationToViewIDIN,
        selected: model.isAuthenticationIDIN(),
        value: model.idinAuthenticationValue()
      };
      if (ff.canUseIDINAuthenticationToView() && isAvailable(idin.value)) {
        options.push(idin);
      }

      return options;
    },

    setPersonalNumber: function (v) {
      var model = this.props.model;
      model.setPersonalNumber(v);
    },

    getPersonalNumberLabelText: function () {
      var model = this.props.model;
      var text = "";
      if (model.isAuthenticationSEBankID()) {
        text = localization.docview.changeAuthenticationToView.ssnSEBankIDLabel;
      } else if (model.isAuthenticationNOBankID()) {
        text = localization.docview.changeAuthenticationToView.ssnNOBankIDLabel;
      } else if (model.isAuthenticationDKNemIDCPR() || model.isAuthenticationDKNemIDPID()) {
        text = localization.docview.changeAuthenticationToView.ssnDKNemIDCPRLabel;
      } else if (model.isAuthenticationDKNemIDCVR()) {
        text = localization.docview.changeAuthenticationToView.ssnDKNemIDCVRLabel;
      } else if (model.isAuthenticationFITupas()) {
        text = localization.docview.changeAuthenticationToView.ssnFITupasLabel;
      }
      return text;
    },

    getPersonalNumberPlaceholderText: function () {
      var model = this.props.model;
      var text = "";
      if (model.isAuthenticationSEBankID()) {
        text = localization.docview.changeAuthenticationToView.ssnSEBankIDPlaceholder;
      } else if (model.isAuthenticationNOBankID()) {
        text = localization.docview.changeAuthenticationToView.ssnNOBankIDPlaceholder;
      } else if (model.isAuthenticationDKNemIDCPR() || model.isAuthenticationDKNemIDPID()) {
        text = localization.docview.changeAuthenticationToView.ssnDKNemIDCPRPlaceholder;
      } else if (model.isAuthenticationDKNemIDCVR()) {
        text = localization.docview.changeAuthenticationToView.ssnDKNemIDCVRPlaceholder;
      } else if (model.isAuthenticationFITupas()) {
        text = localization.docview.changeAuthenticationToView.ssnFITupasPlaceholder;
      }
      return text;
    },

    setMobileNumber: function (v) {
      var model = this.props.model;
      model.setMobileNumber(v);
    },

    getMobileNumberLabelText: function () {
      return localization.docview.changeAuthenticationToView.phoneLabel;
    },

    render: function () {
      var model = this.props.model;
      return (
        <div>
          <label>
            {localization.docview.changeAuthenticationToView.methodLabel}
          </label>
          <Select
            onSelect={this.setAuthenticationMethod}
            width={348}
            options={this.getAuthenticationMethodOptions()}
          />
          {/* if */ (model.isAuthenticationSEBankID() || model.isAuthenticationNOBankID()
                     || model.isAuthenticationDKNemIDCPR() || model.isAuthenticationDKNemIDPID()
                     || model.isAuthenticationDKNemIDCVR() || model.isAuthenticationLegacyDKNemID()
                     || model.isAuthenticationFITupas()) &&
            <div>
              <label>{this.getPersonalNumberLabelText()}</label>
              <InfoTextInput
                inputtype="text"
                infotext={this.getPersonalNumberPlaceholderText()}
                value={model.personalNumber()}
                onChange={this.setPersonalNumber}
                className={classNames({"obligatory-input": !model.isPersonalNumberValid()})}
              />
            </div>
          }
          {/* if */ (model.isAuthenticationNOBankID() || model.isAuthenticationSMSPin()) &&
            <div>
              <label>{this.getMobileNumberLabelText()}</label>
              <InfoTextInput
                inputtype="text"
                value={model.mobileNumber()}
                onChange={this.setMobileNumber}
                className={classNames({"obligatory-input": !model.isMobileNumberValid()})}
              />
            </div>
          }
        </div>
      );
    }
  });

module.exports = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    signatory: React.PropTypes.object.isRequired,
    onClose: React.PropTypes.func.isRequired,
    onAction: React.PropTypes.func.isRequired
  },
  componentWillMount: function () {
    this._model = new ChangeAuthenticationToViewModalModel({
      signatory: this.props.signatory
    });
  },
  onAccept: function () {
    var authenticationMethod = this._model.authenticationMethod();
    var personalNumber = this._model.personalNumber();
    var mobileNumber = this._model.mobileNumber();

    if (!this._model.isValid()) {
      new FlashMessage({
        content: this._model.getInvalidFlashMessageText(),
        type: "error"
      });

      return;
    }

    Track.track_timeout("Accept", {
      "Accept": "Change authentication to view",
      "Signatory index": this._model.signatory().signIndex(),
      "Authentication method": authenticationMethod,
      "Personal number": personalNumber,
      "Mobile number": mobileNumber
    });

    this.props.onClose();
    LoadingDialog.open();

    var self = this;
    this._model.signatory().changeAuthenticationToView(authenticationMethod, personalNumber, mobileNumber).sendAjax(
      function () {
        self.props.onAction();
      },
      function (err) {
        LoadingDialog.close();
        new FlashMessage({
          content: localization.docview.changeAuthenticationToView.errorFlashMessage,
          type: "error"
        });
      }
    );
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active} width={420}>
        <Modal.Header
          title={localization.docview.changeAuthenticationToView.title}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div className="docview-changeauthentication-modal">
            <ChangeAuthenticationToViewModalView model={this._model} />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            text={localization.docview.changeAuthenticationToView.accept}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
