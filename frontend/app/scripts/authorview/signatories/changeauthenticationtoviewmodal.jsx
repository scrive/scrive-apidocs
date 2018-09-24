var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var SSNForSEBankIDValidation = require("../../../js/validation.js").SSNForSEBankIDValidation;
var SSNForNOBankIDValidation = require("../../../js/validation.js").SSNForNOBankIDValidation;
var SSNForDKNemIDValidation = require("../../../js/validation.js").SSNForDKNemIDValidation;
var SSNForFITupasValidation = require("../../../js/validation.js").SSNForFITupasValidation;
var PhoneValidationNO = require("../../../js/validation.js").PhoneValidationNO;
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;
var EmptyValidation = require("../../../js/validation.js").EmptyValidation;
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
        } else if (this.isAuthenticationDKNemID()) {
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

    DKNemIDAuthenticationValue: function () {
      return "dk_nemid";
    },

    isAuthenticationDKNemID: function () {
      return this.authenticationMethod() == this.DKNemIDAuthenticationValue();
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
      } else if (this.isAuthenticationDKNemID()) {
        return new SSNForDKNemIDValidation().validateData(pn);
      } else if (this.isAuthenticationFITupas()) {
        return new SSNForFITupasValidation().validateData(pn);
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
      } else if (this.isAuthenticationDKNemID() && !this.isPersonalNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidDKSSN;
      } else if (this.isAuthenticationFITupas() && !this.isPersonalNumberValid()) {
        text = localization.docview.changeAuthenticationToView.flashMessageInvalidFISSN;
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
      } else if (model.isAuthenticationDKNemID()) {
        return localization.docview.signatory.authenticationToViewDKNemID;
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

      var standard = {
        name: localization.docview.signatory.authenticationToViewStandard,
        selected: model.isAuthenticationStandard(),
        value: model.standardAuthenticationValue()
      };
      if (sig.standardAuthenticationToView() ||
          ff.canUseStandardAuthenticationToView()
         ) {
        options.push(standard);
      }


      var seBankid = {
        name: localization.docview.signatory.authenticationToViewSEBankID,
        selected: model.isAuthenticationSEBankID(),
        value: model.SEBankIDAuthenticationValue()
      };

      if (sig.seBankIDAuthenticationToView() ||
         (ff.canUseSEAuthenticationToView()
           && !sig.dkNemIDAuthenticationToSign()
           && !sig.noBankIDAuthenticationToSign())) {
        options.push(seBankid);
      }

      var noBankid = {
        name: localization.docview.signatory.authenticationToViewNOBankID,
        selected: model.isAuthenticationNOBankID(),
        value: model.NOBankIDAuthenticationValue()
      };

      if (sig.noBankIDAuthenticationToView() ||
         (ff.canUseNOAuthenticationToView()
           && !sig.dkNemIDAuthenticationToSign()
           && !sig.seBankIDAuthenticationToSign())
      ) {
        options.push(noBankid);
      }

      var dkNemid = {
        name: localization.docview.signatory.authenticationToViewDKNemID,
        selected: model.isAuthenticationDKNemID(),
        value: model.DKNemIDAuthenticationValue()
      };

      if (sig.dkNemIDAuthenticationToView() ||
         (ff.canUseDKAuthenticationToView()
          && !sig.seBankIDAuthenticationToSign()
          && !sig.noBankIDAuthenticationToSign())
      ) {
        options.push(dkNemid);
      }

      var fiTupas = {
        name: localization.docview.signatory.authenticationToViewFITupas,
        selected: model.isAuthenticationFITupas(),
        value: model.FITupasAuthenticationValue()
      };

      if (sig.fiTupasAuthenticationToView() ||
         (ff.canUseFIAuthenticationToView()
          && !sig.seBankIDAuthenticationToSign()
          && !sig.noBankIDAuthenticationToSign()
          && !sig.dkNemIDAuthenticationToSign())
      ) {
        options.push(fiTupas);
      }

      var smsPin = {
        name: localization.docview.signatory.authenticationToViewSMSPin,
        selected: model.isAuthenticationSMSPin(),
        value: model.SMSPinAuthenticationValue()
      };

      if (sig.smsPinAuthenticationToView() ||
          ff.canUseSMSPinAuthenticationToView()
      ) {
        options.push(smsPin);
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
      } else if (model.isAuthenticationDKNemID()) {
        text = localization.docview.changeAuthenticationToView.ssnDKNemIDLabel;
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
      } else if (model.isAuthenticationDKNemID()) {
        text = localization.docview.changeAuthenticationToView.ssnDKNemIDPlaceholder;
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
          {/* if */ (!model.isAuthenticationStandard() && !model.isAuthenticationSMSPin()) &&
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
