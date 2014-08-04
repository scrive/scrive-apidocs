/** @jsx React.DOM */

/*
 * Modal for changing authentication method.
 *
 * Uses signatory.changeAuthentication
 * Does some input validation and flash message errors
 * Uses existing values for mobile or personal number where available
 *
 * Example usage:
 *    new ChangeAuthenticationModal({
 *      signatory : ___,
 *      triggerOnAction : ___
 *    });
 *
 */
define(['React','common/backbone_mixin','Backbone','common/select', 'legacy_code'], function(React, BackboneMixin, Backbone, Select) {

var ChangeAuthenticationModalModel = Backbone.Model.extend({
  defaults : {
    onAction : function() {},
    newAuthenticationValueInvalid : false
  },
  initialize: function (args) {
    var self = this;
    this.setNewAuthenticationMethod(this.signatory().authentication());
  },
  triggerOnAction : function() {
    if (this.get("onAction"))
      this.get("onAction")();
  },
  document :function() {
     return this.signatory().document();
  },
  signatory : function() {
     return this.get("signatory");
  },
  newAuthenticationMethod : function() {
    return this.get('newAuthenticationMethod');
  },
  newAuthenticationValue : function() {
    return this.get('newAuthenticationValue');
  },
  newAuthenticationValueInvalid : function() {
    return this.get('newAuthenticationValueInvalid');
  },
  setNewAuthenticationValueInvalid : function(v) {
    this.set({newAuthenticationValueInvalid : v});
  },
  setNewAuthenticationMethod : function (method) {
    if(method != this.newAuthenticationMethod()) {
      var signatory = this.signatory();
      if(this.isNewAuthenticationPINbySMS()) {
        this.setNewAuthenticationValue(signatory.mobile());
      }
      else if(this.isNewAuthenticationELeg()) {
        this.setNewAuthenticationValue(signatory.personalnumber());
      }
      else {
        this.setNewAuthenticationValue('');
      }
    }
    this.set({newAuthenticationMethod : method});
  },
  setNewAuthenticationValue :  function(value) {
    this.set({newAuthenticationValue : value});
  },
  isNewAuthenticationStandard : function() {
    return this.newAuthenticationMethod() == 'standard';
  },
  isNewAuthenticationPINbySMS : function() {
    return this.newAuthenticationMethod() == 'sms_pin';
  },
  isNewAuthenticationELeg : function() {
    return this.newAuthenticationMethod() == 'eleg';
  },
  isAuthenticationValueInvalid : function() {
    var authvalue = this.newAuthenticationValue();
    if(this.isNewAuthenticationPINbySMS()) {
      return ( ! new PhoneValidation().validateData(authvalue) &&
               ! new EmptyValidation().validateData(authvalue)
             );
    }
    else if(this.isNewAuthenticationELeg()) {
      return ( ! new NumberValidation().validateData(authvalue) &&
               ! new EmptyValidation().validateData(authvalue)
             );
    }
    return false;
  },
  getAuthenticationValueInvalidFlashMessageText : function() {
    var text = '';
    if(this.isNewAuthenticationPINbySMS()) {
      text = localization.docview.changeAuthentication.errorPhone;
    }
    else if(this.isNewAuthenticationELeg()) {
      text = localization.docview.changeAuthentication.errorEID;
    }
    return text;
  }
});


var ChangeAuthenticationModalView = React.createClass({
  mixins : [BackboneMixin.BackboneMixin],
  getBackboneModels : function() {
    return [this.props.model];
  },
  propTypes : {
    model : React.PropTypes.object
  },
  getAuthenticationMethodNameText : function() {
    var model = this.props.model;
    if(model.isNewAuthenticationStandard()) {
      return localization.docview.signatory.authenticationStandard;
    }
    else if(model.isNewAuthenticationPINbySMS()) {
      return localization.docview.signatory.authenticationSMSPin;
    }
    else if(model.isNewAuthenticationELeg()) {
      return localization.docview.signatory.authenticationELeg;
    }
  },
  setAuthenticationMethod : function(v) {
    var model = this.props.model;
    model.setNewAuthenticationMethod(v);
    return true;
  },
  setAuthenticationValue : function(event) {
    var model = this.props.model;
    model.setNewAuthenticationValueInvalid(false);
    model.setNewAuthenticationValue(event.target.value);
  },
  getAuthenticationValueLabelText : function() {
    var text = '';
    if(model.isNewAuthenticationPINbySMS()) {
      text = localization.phone;
    }
    else if(model.isNewAuthenticationELeg()) {
      text = localization.docsignview.personalNumberLabel;
    }
    return text;
  },
  getAuthenticationValuePlaceholderText : function() {
    var text = '';
    if(model.isNewAuthenticationPINbySMS()) {
      text = localization.docview.changeAuthentication.placeholderPhone;
    }
    else if(model.isNewAuthenticationELeg()) {
      localization.docview.changeAuthentication.placeholderEID;
    }
    return text;
  },
  render : function() {
   var model = this.props.model;
   var signatory = model.signatory();
   var SelectComponent =  Select.Select;
   var selectLabel = $('<div>').text(localization.docview.changeAuthentication.methodLabel);
   $('.put-person-name',selectLabel).html($('<strong>').text(signatory.smartname()));
   return (
       <div>
           <label dangerouslySetInnerHTML={{__html: selectLabel.html()}} />
           <SelectComponent
                    name={this.getAuthenticationMethodNameText()}
                    onSelect={this.setAuthenticationMethod}
                    textWidth="321px"
                    optionsWidth="348px"
                    options={ [ { name : localization.docview.signatory.authenticationStandard,
                                  value : "standard"
                                }
                              , { name : localization.docview.signatory.authenticationELeg,
                                  value : "eleg"
                                }
                              , { name : localization.docview.signatory.authenticationSMSPin,
                                  value : "sms_pin"
                                }
                              ] }
           />
           {/*if*/ model.newAuthenticationMethod() != 'standard' &&
           <div>
               <label>{this.getAuthenticationValueLabelText()}</label>
               <div className={model.newAuthenticationValueInvalid() ?
                               'info-text-input obligatory-input' : 'info-text-input'}>
                   <input type='text'
                          placeholder={this.getAuthenticationValuePlaceholderText()}
                          value={model.newAuthenticationValue()}
                          onChange={this.setAuthenticationValue}
                   />
               </div>
               <label className='infotext'>{localization.docview.changeAuthentication.valueInfotext}</label>
           </div>
           }
       </div>
   );
  }
});

return function(args) {
  var model = new ChangeAuthenticationModalModel({
    signatory: args.signatory,
    triggerOnAction : args.triggerOnAction});
  var content = $('<div class="docview-changeauthentication-modal">');
  React.renderComponent(ChangeAuthenticationModalView({
      model : model
  }), content[0]);
  new Confirmation({
    title : localization.docview.changeAuthentication.title,
    acceptText : localization.docview.changeAuthentication.accept,
    content : content,
    width : 420,
    onAccept : function() {
      var authmethod = model.newAuthenticationMethod();
      var authvalue = model.newAuthenticationValue();
      if(model.isAuthenticationValueInvalid()) {
          model.setNewAuthenticationValueInvalid(true);
          new FlashMessage({ content : model.getAuthenticationValueInvalidFlashMessageText()
                           , color : 'red' });
          return false;
      }
      trackTimeout('Accept',
          {'Accept' : 'change authentication',
           'Signatory index' : model.signatory.signIndex(),
           'Authentication method' : authmethod,
           'Authentication value' : authvalue});
      LoadingDialog.open();
      model.signatory.changeAuthentication(authmethod, authvalue).sendAjax(
          function() { model.triggerOnAction();}
        , function () {
            LoadingDialog.close();
            new FlashMessage({ content : localization.docview.changeAuthentication.errorSigned
                             , color : 'red' });
        }
      );
      return true;
    }
  });
}
});
