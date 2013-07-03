/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

var ChangePasswordPopupModel = Backbone.Model.extend({
  defaults : {
    oldpassword : "",
    password1 : "",
    password2 : ""
  },
  initialize : function() {
    var self = this;
  },
  user : function() {
     return this.get("user");
  },
  oldpassword : function() {
     return this.get("oldpassword");
  },
  password1 : function() {
     return this.get("password1");
  },
  password2 : function() {
     return this.get("password2");
  },
  lang : function() {
    return this.get("lang");
  },
  setOldPassword : function(v) {
    this.set({"oldpassword" : v} );
  },
  setPassword1 : function(v) {
    this.set({"password1" : v} );
  },
  setPassword2 : function(v) {
    this.set({"password2" : v} );
  },
  savePassword : function(callback) {
    new Submit({
      method : "POST",
      url : "/api/frontend/changepassword",
      oldpassword : this.oldpassword(),
      password  : this.password1(),
      ajax : true,
      ajaxsuccess : function(rs) {
         var resp = JSON.parse(rs);
         if (resp.changed === true) {
            callback();
         }
         else
           new FlashMessage({color: 'red', content : localization.validation.passwordOldPasswordNotValid});
      }
    }).send();
  }
});



var ChangePasswordPopupView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind("reset", this.render);
        this.render();
    },
    passwordSettings : function() {
      // Building frame
      var model = this.model;
      var box = $("<div>");

      var table = $("<table/>");
      box.append(table);

      var oldpasswordinput = $("<input type='password' style='margin:5px 10px' autocomplete='off'/>");
      oldpasswordinput.change(function() {
          model.setOldPassword(oldpasswordinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.oldpassword))).append($("<td/>").append(oldpasswordinput)));

      password1input = $("<input type='password' style='margin:5px 10px'  autocomplete='off'/>");
      password1input.change(function() {
          model.setPassword1(password1input.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.newpassword1))).append($("<td/>").append(password1input)));

      password2input = $("<input type='password' style='margin:5px 10px'  autocomplete='off'/>");
      password2input.change(function() {
          model.setPassword2(password2input.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.newpassword2))).append($("<td/>").append(password2input)));

      return box;
    },
    passwordsAreValid : function() {
        var model = this.model;
        var res = model.password1().validate(new PasswordValidation({callback: function(text,elem,v) {new FlashMessage({color: 'red', content : v.message() })},
                                message: localization.validation.passwordLessThanMinLength,
                                message_max: localization.validation.passwordExceedsMaxLength,
                                message_digits: localization.validation.passwordNeedsLetterAndDigit
        }));
        if (!res) return false;
        if (model.password1() != model.password2())
        {
          new FlashMessage({color: 'red', content : localization.validation.passwordsDontMatch});
          return false;
        }
        return true;


    },
    render: function () {
       var self = this;
       var model = this.model;
       var popup = Confirmation.popup({
          title: localization.account.accountSecurity.passwordSection,
          content: $("<div>").append(this.passwordSettings()),
          onAccept:  function() {
            if (self.passwordsAreValid())
              model.savePassword(function() {popup.close()});
            return false;
        }
       });
    }
});


window.ChangePasswordPopup = function(args) {
          var model = new ChangePasswordPopupModel(args);
          var view =  new ChangePasswordPopupView({model : model});
};

})(window);
