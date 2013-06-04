/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

var SecuritySettingsModel = Backbone.Model.extend({
  initialize : function() {
    var self = this;
    var user = new User();
    this.set({"user" : user});
    user.bind("change:ready",function() {
      self.reset();
    });
    user.bind("reset",function() {
      self.reset();
    });
    user.fetch({cache: false});
    this.reset();
  },
  user : function() {
     return this.get("user");
  },
  ready : function() {
     return this.user().ready();
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
  setLang : function(v) {
    this.set({"lang" : v}, {silent : true} );
    this.trigger("change:lang");
  },
  reset : function() {
    if (!this.ready()) return;
    this.set({
      oldpassword : "",
      password1  : "",
      password2  : "",
      lang       : this.user().lang() != "sv" ?  "en" : "sv"
    }, {silent : true});
    this.trigger("reset");
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
  },
  saveLang : function(callback) {
    new Submit({
      method : "POST",
      url : "/api/frontend/changelanguage",
      lang     : this.lang(),
      ajax : true,
      ajaxsuccess : callback
    }).send();
  },
  passwordNeedSaving: function() {
    return this.oldpassword() != "" || this.password1() != "" || this.password2() != "";
  },

  save : function() {
    var self = this;
    if (self.passwordNeedSaving())
      self.savePassword(function() { self.saveLang(function() {window.location.reload();})});
    else
      self.saveLang(function() {window.location.reload();});
  },
  refresh : function() {    this.user().fetch({cache: false}); this.reset(); }
});



var SecuritySettingsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind("reset", this.render);
        this.render();
    },
    passwordSettings : function() {
      // Building frame
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").text(localization.account.accountSecurity.passwordSection);
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      var table = $("<table/>");
      body.append(table);

      var oldpasswordinput = $("<input type='password' autocomplete='off'/>");
      oldpasswordinput.change(function() {
          model.setOldPassword(oldpasswordinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.oldpassword))).append($("<td/>").append(oldpasswordinput)));

      password1input = $("<input type='password' autocomplete='off'/>");
      password1input.change(function() {
          model.setPassword1(password1input.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.newpassword1))).append($("<td/>").append(password1input)));

      password2input = $("<input type='password' autocomplete='off'/>");
      password2input.change(function() {
          model.setPassword2(password2input.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.newpassword2))).append($("<td/>").append(password2input)));

      return box;
    },
    langSettings : function() {
      // Building frame
      var self = this;
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").text(localization.account.accountSecurity.langSection);
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      var table = $("<table/>");
      body.append(table);

      this.langSelectBox = $("<td/>");
      var updateLangSelect = function() {
         if (self.langSelect != undefined)  self.langSelect.clear();
         self.langSelectBox.empty();
         self.langSelect = new Select({
                             textWidth : "90px",
                             name : model.lang() == "en" ? localization.account.accountSecurity.langEN : localization.account.accountSecurity.langSV,
                             onSelect : function(v) {model.setLang(v);return true;},
                             options:   model.lang() == "en" ? [{name: localization.account.accountSecurity.langSV, value: "sv"}] :
                                                                        [{name: localization.account.accountSecurity.langEN, value: "en"}]
                           });
         self.langSelectBox.append(self.langSelect.el());
      };
      updateLangSelect();
      model.bind("change:lang",updateLangSelect);
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.lang))).append(this.langSelectBox));
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
    saveButton : function() {
      var self = this;
      var model = this.model;
      var box = $("<div class='account-footer'/>");
      var button = Button.init({
        color : "blue",
        size: "small",
        shape: "rounded",
        text : localization.account.accountSecurity.save,
        onClick : function() {
          if (self.customfooter != undefined) {
            model.setFooter(self.customfooter.val());
          }
          if (model.passwordNeedSaving())
          { if (self.passwordsAreValid())
              model.save();
          }
          else
            model.save();
          return false;
        }
      });
      return box.append(button.input());
    },
    render: function () {
       var model = this.model;
       if (!model.ready()) return;
       var container = $(this.el).empty();
       var box = $("<div class='tab-content account'/>");
       container.append(box);

       box.append(this.passwordSettings());
       box.append(this.langSettings());
       box.append(this.saveButton());
       box.append("<div class='clearfix'></div>");
       return this;
    }
});


window.SecuritySettings = function(args) {
          var model = new SecuritySettingsModel();
          var view =  new SecuritySettingsView({model : model, el : $("<div class='tab-container'/>")});
          return {
              refresh : function() {model.refresh();},
              el  : function() {return $(view.el);}
            };
};

})(window);
