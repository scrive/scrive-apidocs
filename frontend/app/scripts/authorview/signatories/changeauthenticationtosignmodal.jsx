var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var PhoneValidationNO = require("../../../js/validation.js").PhoneValidationNO;
var EmptyValidation = require("../../../js/validation.js").EmptyValidation;
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;
var SSNForSEBankIDValidation = require("../../../js/validation.js").SSNForSEBankIDValidation;
var $ = require("jquery");
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;

  var ChangeAuthenticationModalModel = Backbone.Model.extend({
    initialize: function (args) {
      this.setNewAuthenticationMethod(this.signatory().authenticationToSign());
    },

    document:function () {
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
        } else if (this.isNewAuthenticationELeg()) {
          this.setNewAuthenticationValue(signatory.personalnumber());
        } else {
          this.setNewAuthenticationValue("");
        }
      }
    },

    setNewAuthenticationValue:  function (value) {
      this.set({newAuthenticationValue: value});
    },

    isNewAuthenticationStandard: function () {
      return this.newAuthenticationMethod() == "standard";
    },

    isNewAuthenticationPINbySMS: function () {
      return this.newAuthenticationMethod() == "sms_pin";
    },

    isNewAuthenticationELeg: function () {
      return this.newAuthenticationMethod() == "se_bankid";
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
        // Else we need any valid number, or empty
        } else {
          return !(
            new PhoneValidation().validateData(authvalue)
            || new EmptyValidation().validateData(authvalue)
          );
        }
      } else if (this.isNewAuthenticationELeg()) {
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
      }

      return false;
    },

    getAuthenticationValueInvalidFlashMessageText: function () {
      var text = "";

      if (this.isNewAuthenticationPINbySMS()) {
        text = localization.docview.changeAuthentication.errorPhone;
      } else if (this.isNewAuthenticationELeg()) {
        text = localization.docview.changeAuthentication.errorEID;
      }

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
      } else if (model.isNewAuthenticationELeg()) {
        return localization.docview.signatory.authenticationToSignSEBankID;
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
      } else if (model.isNewAuthenticationELeg()) {
        text = localization.docsignview.personalNumberLabel;
      }
      return text;
    },

    getAuthenticationValuePlaceholderText: function () {
      var model = this.props.model;
      var text = "";
      if (model.isNewAuthenticationPINbySMS()) {
        text = localization.docview.changeAuthentication.placeholderPhone;
      } else if (model.isNewAuthenticationELeg()) {
        text = localization.docview.changeAuthentication.placeholderEID;
      }
      return text;
    },

    getAuthenticationOptions: function () {
      var model = this.props.model;
      var sig = model.signatory();

      var standard = {name: localization.docview.signatory.authenticationToSignStandard, value: "standard"};
      var eleg = {name: localization.docview.signatory.authenticationToSignSEBankID, value: "se_bankid"};
      var sms = {name: localization.docview.signatory.authenticationToSignSMSPin, value: "sms_pin"};

      if (sig.authenticationToView() === "no_bankid") {
        return [standard, sms];
      }

      return [standard, eleg, sms];
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
        return model.newAuthenticationMethod() != "standard";
      }
    },

    render: function () {
      var model = this.props.model;

      var authenticationValueClass = React.addons.classSet({
        "obligatory-input": model.isAuthenticationValueInvalid()
      });

      return (
        <div>
          <label>
            {localization.docview.changeAuthentication.methodLabel}
          </label>
          <Select
            name={this.getAuthenticationMethodNameText()}
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
                className={authenticationValueClass}
              />
              <label className="infotext">{localization.docview.changeAuthentication.valueInfotext}</label>
            </div>
          }
        </div>
      );
    }
  });

  module.exports = function (args) {
    var model = new ChangeAuthenticationModalModel({signatory: args.signatory});
    var content = $("<div class='docview-changeauthentication-modal'>");

    React.render(React.createElement(ChangeAuthenticationModalView, {
      model: model
    }), content[0]);

    new Confirmation({
      title: localization.docview.changeAuthentication.title,
      acceptText: localization.docview.changeAuthentication.accept,
      content: content,
      width: 420,
      onAccept: function () {
        var authmethod = model.newAuthenticationMethod();
        var authvalue = model.newAuthenticationValue();
        var personalNumber = model.isNewAuthenticationELeg() ? authvalue : undefined;
        var mobileNumber = model.isNewAuthenticationPINbySMS() ? authvalue : undefined;

        if (model.isAuthenticationValueInvalid()) {
          new FlashMessage({content: model.getAuthenticationValueInvalidFlashMessageText(), type: "error"});
          return false;
        }

        trackTimeout("Accept", {
          "Accept": "change authentication",
           "Signatory index": model.signatory().signIndex(),
           "Authentication method": authmethod,
           "Authentication value": authvalue
        });

        LoadingDialog.open();

        model.signatory().changeAuthenticationToSign(authmethod, personalNumber, mobileNumber).sendAjax(function () {
          args.onAction();
        }, function (err) {
          LoadingDialog.close();
          new FlashMessage({content: localization.docview.changeAuthentication.errorFlashMessage, type: "error"});
        });

        return true;
      }
    });
  }
