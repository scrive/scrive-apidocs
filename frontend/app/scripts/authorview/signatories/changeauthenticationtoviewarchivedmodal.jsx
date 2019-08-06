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
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var $ = require("jquery");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Track = require("../../common/track");
var classNames = require("classnames");
var Subscription = require("../../account/subscription");

var Modal = require("../../common/modal");

  var ChangeAuthenticationModalModel = Backbone.Model.extend({
    initialize: function (args) {
      this.setNewAuthenticationMethod(this.signatory().authenticationToViewArchived());
    },

    document: function () {
      return this.signatory().document();
    },

    signatory: function () {
      return this.get("signatory");
    },

    newAuthenticationMethod: function () {
      return this.get("newAuthenticationMethod");
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
            || this.isNewAuthenticationDKNemID()
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

    isNewAuthenticationDKNemID: function () {
      return this.newAuthenticationMethod() == "dk_nemid";
    },

    isNewAuthenticationFITupas: function () {
      return this.newAuthenticationMethod() == "fi_tupas";
    },

   isNewAuthenticationVerimi: function () {
      return this.newAuthenticationMethod() == "verimi";
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
      } else if (this.isNewAuthenticationDKNemID()) {
        // If DK NemID to view is set, then SSN needs to be valid and not empty
        if (this.signatory().authenticationToView() === "dk_nemid") {
          return !new SSNForDKNemIDValidation().validateData(authvalue);
          // Else valid or empty
        } else {
          return !(
            new SSNForDKNemIDValidation().validateData(authvalue)
              || new EmptyValidation().validateData(authvalue)
          );
        }
      }
      // There is no NOBankID here, because authentication value is empty in
      // that case.
      return false;
    },

    getAuthenticationValueInvalidFlashMessageText: function () {
      var text = "";

      if (this.isNewAuthenticationPINbySMS()) {
        text = localization.docview.changeAuthentication.errorPhone;
      } else if (this.isNewAuthenticationSEBankID() || this.isNewAuthenticationDKNemID()) {
        text = localization.docview.changeAuthentication.errorEID;
      }
      // no NOBankID here, because there is only empty "" authentication value

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
        return localization.docview.signatory.authenticationToViewStandard;
      } else if (model.isNewAuthenticationPINbySMS()) {
        return localization.docview.signatory.authenticationToViewSMSPin;
      } else if (model.isNewAuthenticationSEBankID()) {
        return localization.docview.signatory.authenticationToViewSEBankID;
      } else if (model.isNewAuthenticationNOBankID()) {
        return localization.docview.signatory.authenticationToViewNOBankID;
      } else if (model.isNewAuthenticationDKNemID()) {
        return localization.docview.signatory.authenticationToViewDKNemID;
      } else if (model.isNewAuthenticationFITupas()) {
        return localization.docview.signatory.authenticationToViewFITupas;
      } else if (model.isNewAuthenticationVerimi()) {
        return localization.docview.signatory.authenticationToViewVerimi;
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
      } else if (model.isNewAuthenticationSEBankID()
                 || model.isNewAuthenticationFITupas()) {
        text = localization.docsignview.personalNumberLabel;
      } else if (model.isNewAuthenticationDKNemID()) {
        text = localization.eID.idName.nemId;
      }
      // there is no NOBankID here, because authentication value is empty in that case.
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
      } else if (model.isNewAuthenticationDKNemID()) {
        text = localization.docview.changeAuthentication.placeholderDKEID;
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
                                               sig.authenticationToSign(),
                                               auth);
      };

      var standard = {
        name: localization.docview.signatory.authenticationToViewStandard,
        selected: model.isNewAuthenticationStandard(),
        value: "standard"
      };
      if (ff.canUseStandardAuthenticationToSign() && isAvailable(standard.value)) {
        options.push(standard);
      }

      var sebankid = {
        name: localization.docview.signatory.authenticationToViewSEBankID,
        selected: model.isNewAuthenticationSEBankID(),
        value: "se_bankid"
      };
      if (ff.canUseSEAuthenticationToSign() && isAvailable(sebankid.value)) {
        options.push(sebankid);
      }

      var nobankid = {
        name: localization.docview.signatory.authenticationToViewNOBankID,
        selected: model.isNewAuthenticationNOBankID(),
        value: "no_bankid"
      };
      if (ff.canUseNOAuthenticationToSign() && isAvailable(nobankid.value)) {
        options.push(nobankid);
      }

      var dknemid = {
        name: localization.docview.signatory.authenticationToViewDKNemID,
        selected: model.isNewAuthenticationDKNemID(),
        value: "dk_nemid"
      };
      if (ff.canUseDKAuthenticationToSign() && isAvailable(dknemid.value)) {
        options.push(dknemid);
      }

      var fitupas = {
        name: localization.docview.signatory.authenticationToViewFITupas,
        selected: model.isNewAuthenticationFITupas(),
        value: "fi_tupas"
      };
      if (ff.canUseFIAuthenticationToView() && isAvailable(fitupas.value)) {
        options.push(fitupas);
      }

      var sms = {
        name: localization.docview.signatory.authenticationToViewSMSPin,
        selected: model.isNewAuthenticationPINbySMS(),
        value: "sms_pin"
      };
      if (ff.canUseSMSPinAuthenticationToSign() && isAvailable(sms.value)) {
        options.push(sms);
      }

      var verimi = {
        name: localization.docview.signatory.authenticationToViewVerimi,
        selected: model.isNewAuthenticationVerimi(),
        value: "verimi"
      };
      if (ff.canUseVerimiAuthenticationToView() && isAvailable(verimi.value)) {
        options.push(verimi);
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
      } else if (model.newAuthenticationMethod() == "standard"
          || model.newAuthenticationMethod() == "no_bankid"
          || model.newAuthenticationMethod() == "verimi"
      ) {
        return false;
      } else {
        return true;
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
                          || this._model.isNewAuthenticationDKNemID()
                          || this._model.isNewAuthenticationFITupas())
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
    this._model.signatory().changeAuthenticationToViewArchived(
      authmethod, personalNumber, mobileNumber).sendAjax(
        function () {
          self.props.onAction();
        },
        function (err) {
          LoadingDialog.close();
          new FlashMessage({
            content: localization.docview.changeAuthentication.errorFlashMessage,
            type: "error"
          });
        });
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active} width={420}>
        <Modal.Header
          title={localization.docview.changeAuthenticationToViewArchived.title}
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
