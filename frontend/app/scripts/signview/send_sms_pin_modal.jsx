/** @jsx React.DOM */

define(['React','common/button','common/infotextinput', 'common/backbone_mixin','Backbone', 'tinycolor', 'legacy_code'], function(React, NewButton, NewInfoTextInput, BackboneMixin, Backbone,tinycolor) {



  /* SendSMSPinModalModel is based on signview as model */
  var SendSMSPinModalModel = Backbone.Model.extend({
    defaults : {
       onSend : function() {}
    },
    initialize : function() {
      this.set({phone: this.mobileField().value() || "" });
    },
    onSend : function() {
       this.get("onSend")();
    },
    margin : function() {
        return this.get("margin");
    },
    fast : function() {
        return this.get("fast");
    },
    model : function() {
      return this.get("model");
    },
    document :function() {
      return this.model().document();
    },
    sendSMSPin : function(success) {
      mixpanel.track('Requesting SMS PIN', {
                        documentid: this.document().documentid(),
                        signatoryid : this.document().currentSignatory().signatoryid(),
                        phone: this.phone()
      });
      this.document().requestPin(success, function(xhr) {
        new ReloadDueToErrorModal(xhr)}
      ).send();
    },
    mobileField : function() {
       return this.document().currentSignatory().mobileField();
    },
    phoneCanChange : function() {
       return !this.mobileField().isClosed();
    },
    savePhoneNumber : function() {
      if (this.phoneCanChange())
        this.mobileField().setValue(this.get("phone"));
    },
    hasValidPhone : function() {
      return new PhoneValidation().validateData(this.phone());
    },
    phone: function() {
      return this.get("phone");
    },
    setPhone: function(v) {
      this.set({phone: v});
    }
  });


  var SendSMSPinModalViewContent = React.createClass({
      propTypes: {
        model    : React.PropTypes.object
      },
      mixins: [BackboneMixin.BackboneMixin],
      getBackboneModels : function() {
        return [this.props.model];
      },
      getInitialState: function() {
        return {active: false};
      },
      setPhone : function(value) {
        this.props.model.setPhone(value);
      },
      render: function() {
        var self = this;
        return (
          <div>
             {/*if*/ this.props.model.phoneCanChange() &&
               <div>
                <div>{localization.docsignview.pinSigning.enterPhoneForPinDelivery}</div>
                   <NewInfoTextInput
                      className={"phone-input " + (this.props.model.hasValidPhone() ? "valid " : "") +  (this.state.active ? "active" : "")}
                      style={{width: "200px"}}
                      infotext={localization.phonePlaceholder}
                      type='text' value={this.props.model.phone()}
                      onFocus={function() {self.setState({active :true})}}
                      onBlur={function() {self.setState({active : false})}}
                      onChange={this.setPhone}
                   />
               </div>
             }
             {/*else*/ !this.props.model.phoneCanChange() &&
              <div>
               {localization.docsignview.pinSigning.pinWillBeSentTo}{" "}<strong>{this.props.model.phone()}</strong>
              </div>
             }
         </div>
        );
      }

  });


  var SendSMSPinModalView = Backbone.View.extend({
    initialize: function(args) {
      var self = this;
      _.bindAll(this, 'render');
      this.render();
    },
    onNext : function() {
      var self = this;
      self.model.savePhoneNumber();
      self.model.sendSMSPin(function() {
        self.model.onSend();
        self.modal.clear();
      });
    },
    startModal : function() {
      var self = this;
      var content =$("<div>");
        React.render(React.createElement(SendSMSPinModalViewContent,{
          model: self.model
      }), content[0]);

      var acceptButton =  new Button({
        size:  "small",
        type: "action",
        cssClass: 'accept-sms-pin',
        text: localization.docsignview.pinSigning.next,
        onClick: function() {
          if (self.model.hasValidPhone())
            self.onNext();
          else
            new FlashMessage({type : "error",content : localization.docsignview.pinSigning.invalidPhone});
          }
        });


        self.modal = new Confirmation({
                  title: localization.docsignview.pinSigning.signWithSMSPin,
                  content: content,
                  width: BrowserInfo.isSmallScreen() ? 825 : 424,
                  cssClass: 'sms-pin-modal',
                  signview : self.model.margin() != undefined ? false : true,
                  fast : self.model.fast(),
                  margin : self.model.margin(),
                  onReject : function() { },
                  acceptButton : acceptButton.el()
          });
    },
    render: function() {
      return this;
    }

  });



  /* Exporting main interface SendSMSPinModal */
  return function(args) {
    var model = new SendSMSPinModalModel(args);
    var view = new SendSMSPinModalView({model: model});
    view.startModal();
    return {
      modalAbsoluteTop : function() {return view.modal.absoluteTop();}
    };
  }
});
