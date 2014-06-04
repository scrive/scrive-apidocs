/*
 * Defines the account settings page.
 */
define(['Backbone', 'legacy_code'], function() {

var AccountSettingsModel = Backbone.Model.extend({
  initialize : function() {
    var self = this;
    var user = new User({});
    this.set({"user" : user});
    user.bind("change",function() {
      self.reset();
    });
    this.user().set({"ready" : false}, {silent: true});
    user.fetch({cache: false, processData: true});
    this.reset();
  },
  companyAdmin : function() {
     return this.get("companyAdmin");
  },
  user : function() {
     return this.get("user");
  },
  company : function() {
    return this.user().company();
  },
  ready : function() {
     return this.user().ready();
  },
  fstname : function() {
     return this.get("fstname");
  },
  setFstname : function(v) {
     this.set({"fstname" : v});
  },
  sndname : function() {
     return this.get("sndname");
  },
  setSndname : function(v) {
     this.set({"sndname" : v});
  },
  personnumber : function() {
     return this.get("personnumber");
  },
  setPersonnumber : function(v) {
     this.set({"personnumber" : v});
  },
  email : function() {
     return this.get("email");
  },
  setEmail : function(v) {
     this.set({"email" : v});
  },
  newemail : function() {
     return this.get("newemail");
  },
  setNewEmail : function(v) {
     this.set({"newemail" : v});
  },
  newemailagain : function() {
     return this.get("newemailagain");
  },
  setNewEmailAgain : function(v) {
     this.set({"newemailagain" : v});
  },
  phone : function() {
     return this.get("phone");
  },
  setPhone : function(v) {
     this.set({"phone" : v});
  },
  companyname  : function() {
     return this.get("companyname");
  },
  setCompanyname : function(v) {
     this.set({"companyname" : v});
  },
  companynumber  : function() {
     return this.get("companynumber");
  },
  setCompanynumber : function(v) {
     this.set({"companynumber" : v});
  },
  companyposition  : function() {
     return this.get("companyposition");
  },
  setCompanyposition : function(v) {
     this.set({"companyposition" : v});
  },
  companyaddress  : function() {
     return this.get("companyaddress");
  },
  setCompanyaddress : function(v) {
     this.set({"companyaddress" : v});
  },
  companyzip  : function() {
     return this.get("companyzip");
  },
  setCompanyzip : function(v) {
     this.set({"companyzip" : v});
  },
  companycity  : function() {
     return this.get("companycity");
  },
  setCompanycity : function(v) {
     this.set({"companycity" : v});
  },
  companycountry  : function() {
     return this.get("companycountry");
  },
  setCompanycountry : function(v) {
     this.set({"companycountry" : v});
  },
  companysmsoriginator: function() {
     return this.get("companysmsoriginator");
  },
  setCompanysmsoriginator : function(v) {
     this.set({"companysmsoriginator" : v});
  },
  lang : function() {
    return this.get("lang");
  },
  setLang : function(v) {
    this.set({"lang" : v});
  },
  reset : function() {
    if (!this.ready()) return;
    this.set({
        fstname : this.user().fstname()
      , sndname : this.user().sndname()
      , personnumber : this.user().personalnumber()
      , email : this.user().email()
      , newemail : ""
      , newemailagain : ""
      , phone : this.user().phone()
      , lang :  this.user().lang() ||  "en"
      , companyname :     this.company().companyname()
      , companynumber :   this.company().companynumber()
      , companyposition : this.user().companyposition()
      , companyaddress :  this.company().address()
      , companyzip :      this.company().zip()
      , companycity :     this.company().city()
      , companycountry :   this.company().country()
      , companysmsoriginator : this.company().smsoriginator()
    }, {silent : true});
    this.trigger("reset");
  },
  changeEmail :function(callback) {
     var self = this;
     return new Submit({
      method : "POST",
      url : "/api/frontend/changeemail",
      ajax : true,
      ajaxsuccess : function(rs) {
        var res = JSON.parse(rs);
        if (res.send) {
            var msg = $("<span>" + localization.account.accountDetails.changeEmailMailSent + "</span>");
            msg.find('.email-confirmation-address').text(self.newemail());
            new FlashMessage({content: msg, color: "green"});
        }
        else {
            new FlashMessage({content: localization.account.accountDetails.emailAlreadyInUse, color: "red"});
        }
        if (callback!= undefined) callback();

      },
      newemail : self.newemail()
    }).send();
  },
  updateProfile : function(callback) {
    return new Submit({
      method : "POST",
      url : "/api/frontend/updateprofile",
      ajax : true,
      ajaxsuccess : function() {
        if (callback!= undefined) callback();
      },
      fstname : this.fstname(),
      sndname : this.sndname(),
      personalnumber : this.personnumber(),
      email : this.email(),
      phone : this.phone(),
      lang  : this.lang(),
      companyname :  this.companyname(),
      companynumber :  this.companynumber(),
      companyposition : this.companyposition(),
      companyaddress :  this.companyaddress(),
      companyzip :      this.companyzip(),
      companycity :     this.companycity(),
      companycountry :  this.companycountry(),
      companysmsoriginator : this.companysmsoriginator()
    }).send();
  },
  valid: function() {
    var number = this.companynumber();
    var message = null;
    var validCharsRegex = /^[a-zA-Z0-9-]*$/;
    if (number.length > 50) {
      message = localization.validation.companyNumberTooLong;
    } else if (number.match(validCharsRegex) === null) {
      message = localization.validation.companyNumberInvalidChars;
    }
    if (message !== null) {
      new FlashMessage({content: message, color: "red"});
      return false;
    } else {
      return true;
    }
  },
  save : function() {
    var self  = this;
    if (!self.valid()) {
      return;
    }
    var languageHasChanged = self.user().lang() != self.lang();
    this.updateProfile(function() {
      if (languageHasChanged)
        Language.changeOnCurrentPage(self.lang() , function() {
                new FlashMessage({content : localization.account.accountDetails.detailSaved , color: "blue", withReload: true});
        });
      else {
        new FlashMessage({content : localization.account.accountDetails.detailSaved , color: "blue", withReload: false});
        self.user().fetch({cache: false, processData: true});
      }

    });
  },
  refresh : function() {
    this.user().set({"ready" : false}, {silent: true});
    this.user().fetch({cache: false, processData: true});
    this.reset();
  }
});



var AccountSettingsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind("reset", this.render);
        this.render();
    },
    accountSettings : function() {
      // Building frame
      var self = this;
      var model = this.model;

      var box = $("<div class='blue-box'/>");

      var header = $("<div class='account-header'/>").text(model.user().smartname());
      var body = $("<div class='account-body standard-input-table'/>");
      box.append(header).append(body);


      var table = $("<table/>");
      body.append(table);

      var fstnameinput = $("<input type='text' name='fstname'/>").val(model.fstname());
      fstnameinput.change(function() {
          model.setFstname(fstnameinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.fstname))).append($("<td/>").append(fstnameinput)));

      var sndnameinput = $("<input type='text' name='sndname'/>").val(model.sndname());
      sndnameinput.change(function() {
          model.setSndname(sndnameinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.sndname))).append($("<td/>").append(sndnameinput)));

      var personnumberinput = $("<input type='text' name='personalnumber'/>").val(model.personnumber());
      personnumberinput.change(function() {
          model.setPersonnumber(personnumberinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.personnumber))).append($("<td/>").append(personnumberinput)));

      var emailinput = $("<input type='text' disabled='disabled' class='emailinput' />").val(model.email());
      emailinput.change(function() {
          model.setEmail(emailinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.email))).append($("<td/>").append(emailinput).append(this.changeEmailButton())));

      var passwordinput = $("<input type='text' disabled='disabled' class='newpassword'/>").val("************");
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.passwordSection))).append($("<td/>").append(passwordinput).append(this.changePasswordButton())));


      var phoneinput = $("<input type='text' name='phone'/>").val(model.phone());
      phoneinput.change(function() {
          model.setPhone(phoneinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.phone))).append($("<td/>").append(phoneinput)));

      var companypositioninput = $("<input type='text' name='companyposition'/>").val(model.companyposition());
      companypositioninput.change(function() {
          model.setCompanyposition(companypositioninput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyposition))).append($("<td/>").append(companypositioninput)));

      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountSecurity.lang))).append($("<td/>").append(this.langSelect().el())));


      return box;
    },
    langSelect : function() {
      var self = this;
      var model = this.model;

      var languages = [
          {name: localization.account.accountSecurity.langEN, value: "en"}
        , {name: localization.account.accountSecurity.langSV, value: "sv"}
        , {name: localization.account.accountSecurity.langDE, value: "de", hidden : true}
        , {name: localization.account.accountSecurity.langFR, value: "fr", hidden : true}
        , {name: localization.account.accountSecurity.langIT, value: "it", hidden : true}
        , {name: localization.account.accountSecurity.langES, value: "es", hidden : true}
        , {name: localization.account.accountSecurity.langPT, value: "pt", hidden : true}
        , {name: localization.account.accountSecurity.langNL, value: "nl", hidden : true}
        , {name: localization.account.accountSecurity.langDA, value: "da", hidden : true}
        , {name: localization.account.accountSecurity.langNO, value: "no", hidden : true}
        , {name: localization.account.accountSecurity.langEL, value: "el", hidden : true}
      ];
      var lname = _.findWhere(languages, {value : model.lang()}).name;
      this.langselect = new Select({
                             name : lname,
                             onSelect : function(v) {model.setLang(v); self.langselect.el().replaceWith(self.langSelect().el()); return true;},
                             options: _.filter(languages, function(l) { return l.value !=  model.lang() && !l.hidden;}),
                             textWidth : "213px",
                             optionsWidth : "240px"
                           });
      return this.langselect;
    },
    companySettings : function() {
      // Building frame
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header company'/>").text(model.company().companyname());
      var body = $("<div class='account-body standard-input-table'/>");
      box.append(header).append(body);
      // Data table
            var table = jQuery("<table/>");

            var companynameinput = $("<input type='text' name='companyname'/>").val(model.companyname());
            if (!model.companyAdmin()) companynameinput.attr("disabled","disabled");
            companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyname))).append($("<td/>").append(companynameinput)));

            var companynumberinput = $("<input type='text' name='companynumber'/>").val(model.companynumber());
            if (!model.companyAdmin()) companynumberinput.attr("disabled","disabled");
            companynumberinput.change(function() {
              model.setCompanynumber(companynumberinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companynumber))).append($("<td/>").append(companynumberinput)));

            var companyaddressinput = $("<input type='text' />").val(model.companyaddress());
            if (!model.companyAdmin()) companyaddressinput.attr("disabled","disabled");
            companyaddressinput.change(function() {
              model.setCompanyaddress(companyaddressinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyaddress))).append($("<td/>").append(companyaddressinput)));

            var companyzipinput = $("<input type='text'/>").val(model.companyzip());
            if (!model.companyAdmin()) companyzipinput.attr("disabled","disabled");
            companyzipinput.change(function() {
              model.setCompanyzip(companyzipinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyzip))).append($("<td/>").append(companyzipinput)));

            var companycityinput = $("<input type='text'/>").val(model.companycity());
            if (!model.companyAdmin()) companycityinput.attr("disabled","disabled");
            companycityinput.change(function() {
              model.setCompanycity(companycityinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companycity))).append($("<td/>").append(companycityinput)));

            var companycountryinput = $("<input type='text'/>").val(model.companycountry());
            if (!model.companyAdmin()) companycountryinput.attr("disabled","disabled");
            companycountryinput.change(function() {
              model.setCompanycountry(companycountryinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companycountry))).append($("<td/>").append(companycountryinput)));

            var companysmsoriginatorinput = $("<input type='text' maxlength=11/>").val(model.companysmsoriginator());
            if (!model.companyAdmin()) companysmsoriginatorinput.attr("disabled","disabled");
            companysmsoriginatorinput.change(function() {
              model.setCompanysmsoriginator(companysmsoriginatorinput.val());
            });
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.smsOriginator))).append($("<td/>").append(companysmsoriginatorinput)));
            table.append($("<tr/>").append($("<td/>")).append($("<td/>").append($("<div style='font-size:10px;line-height: 10px;color:#999999;margin:-5px 10px 0px 0px;width:234px;font-style:italic'/>").text(localization.account.accountDetails.smsOriginatorDescription))));


            body.append(table);

      return box;
    },
    changePasswordButton : function() {
      var model = this.model;
      return new Button({
        color: "black",
        text: localization.account.accountDetails.changeEmailButton,
        cssClass : "new-mail-button",
        onClick: function() {
            new ChangePasswordPopup();
          }
      }).el();
    },
    changeEmailButton : function() {
      var model = this.model;
      return new Button({
        color: "black",
        text: localization.account.accountDetails.changeEmailButton,
        cssClass : "new-mail-button",
        onClick: function() {
            mixpanel.track('Click change email button');
            var body = jQuery("<div/>");
            var p = $("<p/>").text(localization.account.accountDetails.changeEmailExplaination);
            body.append(p);
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.account.accountDetails.newEmail ));
            var newemail = jQuery("<input type='text' name='newemail' autocomplete='off' style='margin: 5px 10px;'/>");
            newemail.change(function() {model.setNewEmail(newemail.val());});
            tr1.append(jQuery("<td/>").append(newemail));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.account.accountDetails.newEmailAgain));
            var newemailagain = jQuery("<input type='text' name='newemailagain' autocomplete='off' style='margin: 5px 10px;' />");
            newemailagain.change(function() {model.setNewEmailAgain(newemailagain.val());});
            tr2.append(jQuery("<td/>").append(newemailagain));
            table.append(tr2);

            body.append(table);


            var confirmation = new Confirmation({
              onAccept: function() {
                if ( ! new EmailValidation().validateData(model.newemail())) {
                  new FlashMessage({color: "red", content : localization.account.accountDetails.invalidEmail });
                  return false;
                }
                if (model.newemail() != model.newemailagain()) {
                  new FlashMessage({color: "red", content : localization.account.accountDetails.mismatchedEmails });
                  return false;
                }
                  trackTimeout('Accept',
                               {'Accept' : 'Change email'},
                               function () {
                                   model.changeEmail(
                                     function() {model.updateProfile(function() {
                                       confirmation.close();
                                       model.refresh();
                                    }); }
                                  );
                               });
              },
              onReject : function() {
                  mixpanel.track('Reject',
                                 {'Reject' : 'Change email'});
                    model.setNewEmail("");
                    model.setNewEmailAgain("");
              },
              title: localization.account.accountDetails.changeEmailTitle,
              acceptButtonText: localization.account.accountDetails.changeEmailAccept,
              icon: '/img/modal-icons/change-email.png',
              content: body
            });
            return false;
          }
      }).el();
    },
    saveButton : function() {
      var model = this.model;
      var button = new Button({
        color : "green",
        size: "small",
        cssClass : "save",
        text : localization.account.accountDetails.save,
        onClick : function() {
            mixpanel.track('Click save button');
          model.save();
          return false;
        }
      });
      return button.el();
    },
    render: function () {
       console.log("Rendering main view");
       var model = this.model;
       if (!model.ready()) return;

       var container = $(this.el).empty();
       var box = $("<div class='tab-content account'/>");
       container.append(box);

       box.append(this.accountSettings());
       box.append(this.companySettings());

       var footerbox = $("<div class='account-footer'/>");
       box.append("<div class='clearfix'></div>");
       box.append(footerbox);
       footerbox.append(this.saveButton());

       return this;
    }
});


window.AccountSettings = function(args) {
          var model = new AccountSettingsModel(args);
          var view =  new AccountSettingsView({model : model, el : $("<div class='tab-container account-settings'/>")});
          return {
              refresh : function() {model.refresh();},
              el  : function() {return $(view.el);}
            };
};

});
