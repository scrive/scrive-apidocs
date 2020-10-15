var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var PhoneValidationNO = require("../../../js/validation.js").PhoneValidationNO;
var EmptyValidation = require("../../../js/validation.js").EmptyValidation;
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;
var SSNForSEBankIDValidation = require("../../../js/validation.js").SSNForSEBankIDValidation;
var SSNForDKNemIDValidation = require("../../../js/validation.js").SSNForDKNemIDValidation;
var SSNForFITupasValidation = require("../../../js/validation.js").SSNForFITupasValidation;
var CVRForDKNemIDValidation = require("../../../js/validation.js").CVRForDKNemIDValidation;
var $ = require("jquery");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Track = require("../../common/track");
var classNames = require("classnames");
var Subscription = require("../../account/subscription");

var Modal = require("../../common/modal");

  var ChangeAuthenticationModalModel = Backbone.Model.extend({
    initialize: function (args) {
      this.setNewAuthenticationMethod(this.signatory().authenticationToSign());
    },

    document: function () {
      return this.signatory().document();
    },

    signatory: function () {
      return this.get("signatory");
    },

    newAuthenticationMethod: function () {
      var val = this.get("newAuthenticationMethod");
      if (val === "dk_nemid") {
        val = "dk_nemid_cpr";
      }
      return val;
    },

    newAuthenticationValue: function () {
      return this.get("newAuthenticationValue");
    },

    setNewAuthenticationMethod: function (method) {
      var prvMethod = this.newAuthenticationMethod();
      this.set({newAuthenticationMethod: method});
      if (prvMethod != this.newAuthenticationMethod()) {
        var signatory = this.signatory();
        if (this.isNewAuthenticationPINbySMS()) {
          this.setNewAuthenticationValue(signatory.mobile());
        }
        if (this.isNewAuthenticationSEBankID()
           || this.isNewAuthenticationDKNemIDCPR()
           || this.isNewAuthenticationDKNemIDCVR()
           || this.isNewAuthenticationFITupas()) {
          this.setNewAuthenticationValue(signatory.personalnumber());
        }
        if (this.isNewAuthenticationNOBankID()) {
          this.setNewAuthenticationValue("");
        }
      }
    },

    setNewAuthenticationValue: function (value) {
      this.set({newAuthenticationValue: value});
    },

    isNewAuthenticationStandard: function () {
      return this.newAuthenticationMethod() == "standard";
    },

    isNewAuthenticationPINbySMS: function () {
      return this.newAuthenticationMethod() == "sms_pin";
    },

    isNewAuthenticationSEBankID: function () {
      return this.newAuthenticationMethod() == "se_bankid";
    },

    isNewAuthenticationNOBankID: function () {
      return this.newAuthenticationMethod() == "no_bankid";
    },

    isNewAuthenticationDKNemIDCPR: function () {
      return this.newAuthenticationMethod() == "dk_nemid_cpr";
    },

    isNewAuthenticationDKNemIDPID: function () {
      return this.newAuthenticationMethod() == "dk_nemid_pid";
    },

    isNewAuthenticationDKNemIDCVR: function () {
      return this.newAuthenticationMethod() == "dk_nemid_cvr";
    },

    isNewAuthenticationIDIN: function () {
      return this.newAuthenticationMethod() == "nl_idin";
    },

    isNewAuthenticationFITupas: function () {
      return this.newAuthenticationMethod() == "fi_tupas";
    },

    isNewAuthenticationOnfidoDocumentCheck: function () {
      return this.newAuthenticationMethod() == "onfido_document_check";
    },

    isNewAuthenticationOnfidoDocumentAndPhotoCheck: function () {
      return this.newAuthenticationMethod() == "onfido_document_and_photo_check";
    },

    isNewAuthenticationVerimiQes: function () {
      return this.newAuthenticationMethod() == "verimi_qes";
    },

    isAuthenticationValueInvalid: function () {
      var authvalue = this.newAuthenticationValue();

      if (this.isNewAuthenticationPINbySMS()) {
        // If NO BankID to view is set, then we need a valid Norwegian number, or empty
        if (this.signatory().authenticationToView() === "no_bankid") {
          return !(
            new PhoneValidationNO().validateData(authvalue)
            || new EmptyValidation().validateData(authvalue)
          );
        } else {
          // If SMS Pin to view is set, then phone number needs to be valid and
          // not empty.
          if (this.signatory().authenticationToView() === "sms_pin")
            return !new PhoneValidation().validateData(authvalue);
          else
            return !(
              new PhoneValidation().validateData(authvalue)
                || new EmptyValidation().validateData(authvalue)
            );
        }
      } else if (this.isNewAuthenticationSEBankID()) {
        // If SE BankID to view is set, then SSN needs to be valid and not empty
        if (this.signatory().authenticationToView() === "se_bankid") {
          return !new SSNForSEBankIDValidation().validateData(authvalue);
          // Else valid or empty
        } else {
          return !(
            new SSNForSEBankIDValidation().validateData(authvalue)
            || new EmptyValidation().validateData(authvalue)
          );
        }
      } else if (this.isNewAuthenticationDKNemIDCPR()) {
        // Since we don't allow to enter CPR before signing, it has to be non-empty and valid
        return !new SSNForDKNemIDValidation().validateData(authvalue);
      } else if (this.isNewAuthenticationDKNemIDCVR()) {
        // Since we don't allow to enter CVR before signing, it has to be non-empty and valid
        return !new CVRForDKNemIDValidation().validateData(authvalue);
      } else if (this.isNewAuthenticationFITupas()) {
        // If FI Tupas to view is set, then SSN needs to be valid and not empty
        if (this.signatory().authenticationToView() === "fi_tupas") {
          return !new SSNForFITupasValidation().validateData(authvalue);
          // Else valid or empty
        } else {
          return !(
            new SSNForFITupasValidation().validateData(authvalue)
              || new EmptyValidation().validateData(authvalue)
          );
        }
      }
      return false;
    },

    getAuthenticationValueInvalidFlashMessageText: function () {
      var text = "";

      if (this.isNewAuthenticationPINbySMS()) {
        text = localization.docview.changeAuthentication.errorPhone;
      } else if (this.isNewAuthenticationSEBankID() || this.isNewAuthenticationDKNemIDCPR()
                || this.isNewAuthenticationFITupas()) {
        text = localization.docview.changeAuthentication.errorEID;
      } else if (this.isNewAuthenticationDKNemIDCVR()) {
        text = localization.docview.changeAuthentication.errorCVR;
      }
      // no NOBankID/IDIN/Onfido here, because there is only empty "" authentication value

      return text;
    }
  });

  var ChangeAuthenticationModalView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels: function () {
      return [this.props.model];
    },

    propTypes: {
      model: React.PropTypes.object
    },

    getAuthenticationMethodNameText: function () {
      var model = this.props.model;
      if (model.isNewAuthenticationStandard()) {
        return localization.docview.signatory.authenticationToSignStandard;
      } else if (model.isNewAuthenticationPINbySMS()) {
        return localization.docview.signatory.authenticationToSignSMSPin;
      } else if (model.isNewAuthenticationSEBankID()) {
        return localization.docview.signatory.authenticationToSignSEBankID;
      } else if (model.isNewAuthenticationNOBankID()) {
        return localization.docview.signatory.authenticationToSignNOBankID;
      } else if (model.isNewAuthenticationDKNemIDCPR()) {
        return localization.docview.signatory.authenticationToSignDKNemIDCPR;
      } else if (model.isNewAuthenticationDKNemIDPID()) {
        return localization.docview.signatory.authenticationToSignDKNemIDPID;
      } else if (model.isNewAuthenticationDKNemIDCVR()) {
        return localization.docview.signatory.authenticationToSignDKNemIDCVR;
      } else if (model.isNewAuthenticationIDIN()) {
        return localization.docview.signatory.authenticationToSignIDIN;
      } else if (model.isNewAuthenticationFITupas()) {
        return localization.docview.signatory.authenticationToSignFITupas;
      } else if (model.isNewAuthenticationOnfidoDocumentCheck()) {
        return localization.docview.signatory.authenticationToSignOnfidoDocumentCheck;
      } else if (model.isNewAuthenticationOnfidoDocumentAndPhotoCheck()) {
        return localization.docview.signatory.authenticationToSignOnfidoDocumentAndPhotoCheck;
      } else if (model.isNewAuthenticationVerimiQes()) {
        return localization.docview.signatory.authenticationToSignVerimiQes;
      }
    },

    setAuthenticationMethod: function (v) {
      var model = this.props.model;
      model.setNewAuthenticationMethod(v);
    },

    setAuthenticationValue: function (v) {
      var model = this.props.model;
      model.setNewAuthenticationValue(v);
    },

    getAuthenticationValueLabelText: function () {
      var model = this.props.model;
      var text = "";
      if (model.isNewAuthenticationPINbySMS()) {
        text = localization.phone;
      } else if (model.isNewAuthenticationSEBankID() || model.isNewAuthenticationFITupas()) {
        text = localization.docsignview.personalNumberLabel;
      } else if (model.isNewAuthenticationDKNemIDCPR()) {
        text = localization.eID.idName.cpr;
      } else if (model.isNewAuthenticationDKNemIDCVR()) {
        text = localization.eID.idName.cvr;
      }
      // there is no NOBankID/IDIN/Onfido here, because authentication value is empty in that case.
      return text;
    },

    getAuthenticationValuePlaceholderText: function () {
      var model = this.props.model;
      var text = "";
      if (model.isNewAuthenticationPINbySMS()) {
        text = localization.docview.changeAuthentication.placeholderPhone;
      } else if (model.isNewAuthenticationSEBankID()) {
        text = localization.docview.changeAuthentication.placeholderSEEID;
      } else if (model.isNewAuthenticationNOBankID()) {
        text = localization.docview.changeAuthentication.placeholderNOEID;
      } else if (model.isNewAuthenticationDKNemIDCPR()) {
        text = localization.docview.changeAuthentication.placeholderDKEID;
      } else if (model.isNewAuthenticationDKNemIDCVR()) {
        text = localization.docview.changeAuthentication.placeholderCVR;
      } else if (model.isNewAuthenticationFITupas()) {
        text = localization.docview.changeAuthentication.placeholderFITupas;
      }
      return text;
    },

    getAuthenticationOptions: function () {
      var ff = Subscription.currentSubscription().currentUserFeatures();
      var model = this.props.model;
      var sig = model.signatory();
      var options = [];

      var isAvailable = function (auth) {
        return sig.authenticationMethodsCanMix(sig.authenticationToView(),
                                               auth,
                                               sig.authenticationToViewArchived());
      };

      var standard = {
        name: localization.docview.signatory.authenticationToSignStandard,
        selected: model.isNewAuthenticationStandard(),
        value: "standard"
      };
      if (ff.canUseStandardAuthenticationToSign() && isAvailable(standard.value)) {
        options.push(standard);
      }

      var sebankid = {
        name: localization.docview.signatory.authenticationToSignSEBankID,
        selected: model.isNewAuthenticationSEBankID(),
        value: "se_bankid"
      };
      if (ff.canUseSEAuthenticationToSign() && isAvailable(sebankid.value)) {
        options.push(sebankid);
      }

      var nobankid = {
        name: localization.docview.signatory.authenticationToSignNOBankID,
        selected: model.isNewAuthenticationNOBankID(),
        value: "no_bankid"
      };
      if (ff.canUseNOAuthenticationToSign() && isAvailable(nobankid.value)) {
        options.push(nobankid);
      }

      var dkNemIDCPR = {
        name: localization.docview.signatory.authenticationToSignDKNemIDCPR,
        selected: model.isNewAuthenticationDKNemIDCPR(),
        value: "dk_nemid_cpr"
      };
      if (ff.canUseDKCPRAuthenticationToSign() && isAvailable(dkNemIDCPR.value)) {
        options.push(dkNemIDCPR);
      }

      var dkNemIDPID = {
        name: localization.docview.signatory.authenticationToSignDKNemIDPID,
        selected: model.isNewAuthenticationDKNemIDPID(),
        value: "dk_nemid_pid"
      };
      if (ff.canUseDKPIDAuthenticationToSign() && isAvailable(dkNemIDPID.value)) {
        options.push(dkNemIDPID);
      }

      var dkNemIDCVR = {
        name: localization.docview.signatory.authenticationToSignDKNemIDCVR,
        selected: model.isNewAuthenticationDKNemIDCVR(),
        value: "dk_nemid_cvr"
      };
      if (ff.canUseDKCVRAuthenticationToSign() && isAvailable(dkNemIDCVR.value)) {
        options.push(dkNemIDCVR);
      }

      var sms = {
        name: localization.docview.signatory.authenticationToSignSMSPin,
        selected: model.isNewAuthenticationPINbySMS(),
        value: "sms_pin"
      };
      if (ff.canUseSMSPinAuthenticationToSign() && isAvailable(sms.value)) {
        options.push(sms);
      }

      var idin = {
        name: localization.docview.signatory.authenticationToSignIDIN,
        selected: model.isNewAuthenticationIDIN(),
        value: "nl_idin"
      };
      if (ff.canUseIDINAuthenticationToSign() && isAvailable(idin.value)) {
        options.push(idin);
      }

      var tupas = {
        name: localization.docview.signatory.authenticationToSignFITupas,
        selected: model.isNewAuthenticationFITupas(),
        value: "fi_tupas"
      };
      if (ff.canUseFIAuthenticationToSign() && isAvailable(tupas.value)) {
        options.push(tupas);
      }

      var onfidodoc = {
        name: localization.docview.signatory.authenticationToSignOnfidoDocumentCheck,
        selected: model.isNewAuthenticationOnfidoDocumentCheck(),
        value: "onfido_document_check"
      };
      if (ff.canUseOnfidoAuthenticationToSign() && isAvailable(onfidodoc.value)) {
        options.push(onfidodoc);
      }

      var onfidodocandphoto = {
        name: localization.docview.signatory.authenticationToSignOnfidoDocumentAndPhotoCheck,
        selected: model.isNewAuthenticationOnfidoDocumentAndPhotoCheck(),
        value: "onfido_document_and_photo_check"
      };
      if (ff.canUseOnfidoAuthenticationToSign() && isAvailable(onfidodocandphoto.value)) {
        options.push(onfidodocandphoto);
      }

      var verimiqes = {
        name: localization.docview.signatory.authenticationToSignVerimiQes,
        selected: model.isNewAuthenticationVerimiQes(),
        value: "verimi_qes"
      };
      if (ff.canUseVerimiQesToSign() && isAvailable(verimiqes.value)) {
        options.push(verimiqes);
      }
      return options;
    },

    showAuthenticationValueField: function () {
      var model = this.props.model;
      var signatory = model.signatory();

      // If we are setting SE BankID and signatory has authenticated to view,
      // we cannot change SSN
      if (signatory.hasAuthenticatedToView() && model.newAuthenticationMethod() == "se_bankid") {
          return false;
      // If we are setting SMS PIN and signatory has authenticated to view
      // using NO BankID with a valid mobile, we cannot change phone number
      } else if (signatory.hasAuthenticatedToView()
          && model.newAuthenticationMethod() == "sms_pin"
          && signatory.authenticationToView() == "no_bankid"
          && signatory.mobile() != ""
      ) {
        return false;
      // Else always show if we are setting anything other than no authentication
      } else {
        return (model.newAuthenticationMethod() !== "standard"
          && model.newAuthenticationMethod() !== "no_bankid"
          && model.newAuthenticationMethod() !== "nl_idin");
      }
    },

    render: function () {
      var model = this.props.model;

      return (
        <div>
          <label>
            {localization.docview.changeAuthentication.methodLabel}
          </label>
          <Select
            onSelect={this.setAuthenticationMethod}
            width={348}
            options={this.getAuthenticationOptions()}
          />
          {/* if */ this.showAuthenticationValueField() &&
            <div>
              <label>{this.getAuthenticationValueLabelText()}</label>
              <InfoTextInput
                inputtype="text"
                infotext={this.getAuthenticationValuePlaceholderText()}
                value={model.newAuthenticationValue()}
                onChange={this.setAuthenticationValue}
                className={classNames({"obligatory-input": model.isAuthenticationValueInvalid()})}
              />
              <label className="infotext">{localization.docview.changeAuthentication.valueInfotext}</label>
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
    this._model = new ChangeAuthenticationModalModel({
      signatory: this.props.signatory
    });
  },
  onAccept: function () {
    var authmethod = this._model.newAuthenticationMethod();
    var authvalue = this._model.newAuthenticationValue();
    var personalNumber = (this._model.isNewAuthenticationSEBankID()
                          || this._model.isNewAuthenticationDKNemIDCPR()
                          || this._model.isNewAuthenticationDKNemIDCVR())
                         ? authvalue : undefined;
    var mobileNumber = this._model.isNewAuthenticationPINbySMS() ? authvalue : undefined;

    if (this._model.isAuthenticationValueInvalid()) {
      new FlashMessage({
        content: this._model.getAuthenticationValueInvalidFlashMessageText(),
        type: "error"
      });

      return;
    }

    Track.track_timeout("Accept", {
      "Accept": "change authentication",
      "Signatory index": this._model.signatory().signIndex(),
      "Authentication method": authmethod,
      "Authentication value": authvalue
    });

    this.props.onClose();
    LoadingDialog.open();

    var self = this;
    this._model.signatory().changeAuthenticationToSign(authmethod, personalNumber, mobileNumber).sendAjax(
      function () {
        self.props.onAction();
      },
      function (err) {
        LoadingDialog.close();
        new FlashMessage({
          content: localization.docview.changeAuthentication.errorFlashMessage,
          type: "error"
        });
      }
    );
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active} width={420}>
        <Modal.Header
          title={localization.docview.changeAuthentication.title}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div className="docview-changeauthentication-modal">
            <ChangeAuthenticationModalView model={this._model} />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            text={localization.docview.changeAuthentication.accept}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});
