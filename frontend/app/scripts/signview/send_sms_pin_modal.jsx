/** @jsx React.DOM */

define(['React','common/button','common/backbone_mixin','Backbone', 'tinycolor', 'legacy_code'], function(React, NewButton, BackboneMixin, Backbone,tinycolor) {



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
      this.document().requestPin(success, function(xhr) {new OpenSigningFailedAndReloadModal(xhr);}).send();
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
    },
    usebranding : function() {
      return this.model().usebranding();
    },
    signviewbranding : function() {
      return this.model().signviewbranding();
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
        return {focus: false, hover : false};
      },
      setPhone : function(event) {
        this.props.model.setPhone(event.target.value);
      },
      borderColor: function() {
        var model = this.props.model;
        var color = model.usebranding() && model.signviewbranding().signviewprimarycolour() ? model.signviewbranding().signviewprimarycolour() : '#53b688';
        var standardBorderColor = tinycolor(color);
        standardBorderColor.setAlpha(0.6);
        var highlightedBorderColor = tinycolor(color);
        highlightedBorderColor.setAlpha(1);
        var validBorderColor = '#ddd';
        if (this.props.model.hasValidPhone())
           return validBorderColor;
        if (this.state.focus || this.state.hover)
          return highlightedBorderColor;
        return standardBorderColor;

      },
      inputOnFocus: function() {
        this.setState({focus : true});
      },
      inputOnBlur : function() {
        this.setState({focus : false});
      },
      inputOnMouseEnter : function() {
        this.setState({hover : true});
      },
      inputOnMouseLeave : function() {
        this.setState({hover : false});
      },
      render: function() {
        return (
          <div>
             {/*if*/ this.props.model.phoneCanChange() &&
               <div>
                <div>{localization.docsignview.pinSigning.enterPhoneForPinDelivery}</div>
                  <div className="info-text-input"  style={{borderColor:this.borderColor()}}>
                     <input
                        style={{width: "200px"}}
                        onMouseEnter={this.inputOnMouseEnter}
                        onMouseLeave={this.inputOnMouseLeave}
                        onFocus={this.inputOnFocus}
                        onBlur={this.inputOnBlur}
                        placeholder={localization.phonePlaceholder}
                        type='text' value={this.props.model.phone()}
                        onChange={this.setPhone}
                     />
                 </div>
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
        React.renderComponent(SendSMSPinModalViewContent({
          model: self.model
      }), content[0]);

      var acceptButton =  new Button({
        size:  "small",
        color: "green",
        customcolor: this.model.usebranding() ? this.model.signviewbranding().signviewprimarycolour() : undefined,
        textcolor: this.model.usebranding() ? this.model.signviewbranding().signviewprimarytextcolour() : undefined,
        cssClass: 'greybg',
        text: localization.docsignview.pinSigning.next,
        onClick: function() {
          if (self.model.hasValidPhone())
            self.onNext();
          else
            new FlashMessage({color: "red",content : localization.docsignview.pinSigning.invalidPhone});
          }
        });


        self.modal = new Confirmation({
                  title: localization.docsignview.pinSigning.signWithSMSPin,
                  content: content,
                  width: BrowserInfo.isSmallScreen() ? 825 : 424,
                  cssClass: 'grey',
                  signview : self.model.margin() != undefined ? false : true,
                  fast : self.model.fast(),
                  margin : self.model.margin(),
                  textcolor : this.model.usebranding() ? this.model.signviewbranding().signviewtextcolour() : undefined,
                  textfont : this.model.usebranding() ? this.model.signviewbranding().signviewtextfont() : undefined,
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
