var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var SSNForSEBankIDValidation = require("../../../js/validation.js").SSNForSEBankIDValidation;
var SSNForNOBankIDValidation = require("../../../js/validation.js").SSNForNOBankIDValidation;
var PhoneValidationNO = require("../../../js/validation.js").PhoneValidationNO;
var EmptyValidation = require("../../../js/validation.js").EmptyValidation;
var $ = require("jquery");
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var trackTimeout = require("../../common/track_timeout");
var classNames = require("classnames");

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

    canUseNOBankID: function () {
      return this.signatory().authenticationToSign() != "se_bankid";
    },

    isAuthenticationNOBankID: function () {
      return this.authenticationMethod() == this.NOBankIDAuthenticationValue();
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
      if (this.isAuthenticationNOBankID()) {
        var m = this.mobileNumber();
        return (
          new PhoneValidationNO().validateData(m) ||
          new EmptyValidation().validateData(m)
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
      }
    },

    getAuthenticationMethodOptions: function () {
      var model = this.props.model;

      var standard = {
        name: localization.docview.signatory.authenticationToViewStandard,
        selected: model.isAuthenticationStandard(),
        value: model.standardAuthenticationValue()
      };
      var seBankid = {
        name: localization.docview.signatory.authenticationToViewSEBankID,
        selected: model.isAuthenticationSEBankID(),
        value: model.SEBankIDAuthenticationValue()
      };
      var noBankid = {
        name: localization.docview.signatory.authenticationToViewNOBankID,
        selected: model.isAuthenticationNOBankID(),
        value: model.NOBankIDAuthenticationValue()
      };

      if (model.canUseNOBankID()) {
        return [standard, seBankid, noBankid];
      } else {
        return [standard, seBankid];
      }
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
          {/* if */ !model.isAuthenticationStandard() &&
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
          {/* if */ model.isAuthenticationNOBankID() &&
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

  module.exports = function (args) {
    var model = new ChangeAuthenticationToViewModalModel({signatory: args.signatory});
    var content = $("<div class='docview-changeauthentication-modal'>");

    React.render(React.createElement(ChangeAuthenticationToViewModalView, {
      model: model
    }), content[0]);

    new Confirmation({
      title: localization.docview.changeAuthenticationToView.title,
      acceptText: localization.docview.changeAuthenticationToView.accept,
      content: content,
      width: 420,
      onAccept: function () {
        var authenticationMethod = model.authenticationMethod();
        var personalNumber = model.personalNumber();
        var mobileNumber = model.mobileNumber();

        if (!model.isValid()) {
          new FlashMessage({content: model.getInvalidFlashMessageText(), type: "error"});
          return false;
        }

        trackTimeout("Accept", {
          "Accept": "Change authentication to view",
          "Signatory index": model.signatory().signIndex(),
          "Authentication method": authenticationMethod,
          "Personal number": personalNumber,
          "Mobile number": mobileNumber
        });

        LoadingDialog.open();

        model.signatory().changeAuthenticationToView(authenticationMethod, personalNumber, mobileNumber)
          .sendAjax(function () {
            args.onAction();
            }, function (err) {
              LoadingDialog.close();
              new FlashMessage({
                content: localization.docview.changeAuthenticationToView.errorFlashMessage,
                type: "error"
              });
          });

        return true;
      }
    });
  };
