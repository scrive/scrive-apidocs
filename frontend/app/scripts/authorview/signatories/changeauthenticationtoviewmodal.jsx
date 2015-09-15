/** @jsx React.DOM */

define(["React", "common/backbone_mixin", "Backbone", "common/select", "legacy_code"],
  function (React, BackboneMixin, Backbone, Select) {

  var ChangeAuthenticationToViewModalModel = Backbone.Model.extend({
    defaults: {
    },

    initialize: function (args) {
      this.setAuthenticationMethod(this.signatory().authenticationToView());
    },

    document:function () {
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

    standardAuthenticationValue : function () {
      return "standard";
    },

    isAuthenticationStandard: function () {
      return this.authenticationMethod() == this.standardAuthenticationValue();
    },

    SEBankIDAuthenticationValue : function () {
      return "se_bankid";
    },

    isAuthenticationSEBankID: function () {
      return this.authenticationMethod() == this.SEBankIDAuthenticationValue();
    },

    NOBankIDAuthenticationValue : function () {
      return "no_bankid";
    },

    canUseNOBankID: function () {
      return this.signatory().authenticationToSign() != "eleg";
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
        m = this.mobileNumber();
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

      var standard = {name: localization.docview.signatory.authenticationToViewStandard,
        value: model.standardAuthenticationValue()};
      var se_bankid = {name: localization.docview.signatory.authenticationToViewSEBankID,
        value: model.SEBankIDAuthenticationValue()};
      var no_bankid = {name: localization.docview.signatory.authenticationToViewNOBankID,
        value: model.NOBankIDAuthenticationValue()};

      if (model.canUseNOBankID()) {
        return [standard, se_bankid, no_bankid];
      }
      else {
        return [standard, se_bankid];
      }
    },

    setPersonalNumber: function (e) {
      var model = this.props.model;
      model.setPersonalNumber(e.target.value);
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
      if (model.isAuthenticationNOBankID()) {
        text = localization.docview.changeAuthenticationToView.ssnNOBankIDPlaceholder;
      }
      return text;
    },

    setMobileNumber: function(e) {
      var model = this.props.model;
      model.setMobileNumber(e.target.value);
    },

    getMobileNumberLabelText: function () {
      return localization.docview.changeAuthenticationToView.phoneLabel;
    },

    render: function () {
      var model = this.props.model;
      var signatory = model.signatory();
      var methodLabel = $("<div>").html(localization.docview.changeAuthenticationToView.methodLabel);

      return (
        <div>
          <label dangerouslySetInnerHTML={{__html: methodLabel.html()}} />
          <Select
            name={this.getAuthenticationMethodNameText()}
            onSelect={this.setAuthenticationMethod}
            width={348}
            options={this.getAuthenticationMethodOptions()}
          />
          {/* if */ !model.isAuthenticationStandard() &&
            <div>
              <label>{this.getPersonalNumberLabelText()}</label>
              <div className={model.isPersonalNumberValid() ?
                  "info-text-input" : "info-text-input obligatory-input"
                }
              >
                <input
                  type="text"
                  placeholder={this.getPersonalNumberPlaceholderText()}
                  value={model.personalNumber()}
                  onChange={this.setPersonalNumber}
                />
              </div>
            </div>
          }
          {/* if */ model.isAuthenticationNOBankID() &&
            <div>
              <label>{this.getMobileNumberLabelText()}</label>
              <div className={model.isMobileNumberValid() ?
                  "info-text-input" : "info-text-input obligatory-input"
                }
              >
                <input
                  type="text"
                  value={model.mobileNumber()}
                  onChange={this.setMobileNumber}
                />
              </div>
            </div>
          }
        </div>
      );
    }
  });

  return function (args) {
    var model = new ChangeAuthenticationToViewModalModel({signatory: args.signatory});
    var content = $("<div class=\"docview-changeauthentication-modal\">");

    React.render(React.createElement(ChangeAuthenticationToViewModalView, {
      model: model
    }), content[0]);

    new Confirmation({
      title: localization.docview.changeAuthenticationToView.title,
      acceptText: localization.docview.changeAuthenticationToView.accept,
      content: content,
      width: 420,
      onAccept: function () {
        var authentication_method = model.authenticationMethod();
        var personal_number = model.personalNumber();
        var mobile_number = model.mobileNumber();

        if (!model.isValid()) {
          new FlashMessage({content: model.getInvalidFlashMessageText(), type: "error"});
          return false;
        }

        trackTimeout("Accept", {
          "Accept": "Change authentication to view",
          "Signatory index": model.signatory().signIndex(),
          "Authentication method": authentication_method,
          "Personal number": personal_number,
          "Mobile number": mobile_number
        });

        LoadingDialog.open();

        model.signatory().changeAuthenticationToView(authentication_method, personal_number, mobile_number)
          .sendAjax(function () {
            args.onAction();}, function (err) {
              LoadingDialog.close();
              new FlashMessage({content: localization.docview.changeAuthenticationToView.errorFlashMessage, type: "error"});
          });

        return true;
      }
    });
  }
});
