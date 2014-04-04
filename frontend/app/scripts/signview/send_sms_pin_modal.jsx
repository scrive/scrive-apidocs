/** @jsx React.DOM */

define(['React','common/button','common/backbone_mixim','Backbone', 'legacy_code'], function(React, Button, BackboneMixin, Backbone) {



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
    newdaystoremind: function() {
      return this.get("newdaystoremind");
    },
    model : function() {
      return this.get("model");
    },
    document :function() {
      return this.model().document();
    },
    sendSMSPin : function(success) {
      this.document().requestPin(success).send();
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
        model    : React.PropTypes.object,
      },
      mixins: [BackboneMixin.BackboneMixin],
      getBackboneModels : function() {
        return [this.props.model];
      },
      setPhone : function(event) {
        this.props.model.setPhone(event.target.value);
      },
      render: function() {
        return (
          <div>
             {/*if*/ this.props.model.phoneCanChange() &&
               <div>
                Phone number for SMS PIN delivery
                <input style={{width: "100px"}}  type='text' value={this.props.model.phone()} onChange={this.setPhone}/>
               </div>
             }
             {/*else*/ !this.props.model.phoneCanChange() &&
              <div>
                SMS pin code has been send to <strong>{this.props.model.phone()}</strong>
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
    /*sentButton : function() {
      var self = this;
      return new Button({
        color: "green",
        style : (BrowserInfo.isSmallScreen() ? "margin-top:-10px" : ""),
        size: "small",
        text : (self.model.document().autoremindtime() != undefined ? localization.autoreminders.changeAutoreminderButton : localization.autoreminders.setAutoreminderButton),
        onClick : function() {
          if (self.model.newdaystoremind() == undefined) return; // This should never happend;
          self.model.setautoreminder(self.model.newdaystoremind(),function() {
            self.modal.close();
          });
        }
      }).el();
    },*/
    onNext : function() {
      var self = this;
      self.model.savePhoneNumber();
      LoadingDialog.open("Sending SMS with PIN");
      self.model.sendSMSPin(function() {
        LoadingDialog.close();
        self.modal.close();
        self.model.onSend();
      });
    },
    startModal : function() {
        var self = this;
        var content =$("<div>");
         React.renderComponent(SendSMSPinModalViewContent({
          model: self.model
        }), content[0]);
        self.modal = new Confirmation({
                  title: "Sign with SMS PIN",
                  //subtitle : $("<div/>").html(localization.autoreminders.changeAutoreminderDescription),
                  content: content,
                  width: 424,
                  //icon : '/img/modal-icons/extend-duedate.png',
                  onReject : function() { },
                  onAccept : function() {
                    if (self.model.hasValidPhone())
                      self.onNext()
                    else
                      new FlashMessage({color: "red",content : "Invalid phone"});
                  },
                  acceptText : "Next"
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
  }
});
