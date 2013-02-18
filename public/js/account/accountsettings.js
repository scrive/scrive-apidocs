/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

var AccountSettingsModel = Backbone.Model.extend({
  initialize : function() {
    var self = this;
    var user = new User();
    this.set({"user" : user})
    user.bind("change:ready",function() {
      self.reset();
    });
    user.bind("reset",function() {
      console.log('')
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
     this.set({"fstname" : v})
  },
  sndname : function() {
     return this.get("sndname");
  },
  setSndname : function(v) {
     this.set({"sndname" : v})
  },
  personnumber : function() {
     return this.get("personnumber");
  },
  setPersonnumber : function(v) {
     this.set({"personnumber" : v})
  },
  email : function() {
     return this.get("email");
  },
  setEmail : function(v) {
     this.set({"email" : v})
  },
  newemail : function() {
     return this.get("newemail");
  },
  setNewEmail : function(v) {
     this.set({"newemail" : v})
  },
  newemailagain : function() {
     return this.get("newemailagain");
  },
  setNewEmailAgain : function(v) {
     this.set({"newemailagain" : v})
  },
  phone : function() {
     return this.get("phone");
  },
  setPhone : function(v) {
     this.set({"phone" : v})
  },
  companyname  : function() {
     return this.get("companyname");
  },
  setCompanyname : function(v) {
     this.set({"companyname" : v})
  },
  companynumber  : function() {
     return this.get("companynumber");
  },
  setCompanynumber : function(v) {
     this.set({"companynumber" : v})
  },
  companyposition  : function() {
     return this.get("companyposition");
  },
  setCompanyposition : function(v) {
     this.set({"companyposition" : v})
  },
  companyaddress  : function() {
     return this.get("companyaddress");
  },
  setCompanyaddress : function(v) {
     this.set({"companyaddress" : v})
  },
  companyzip  : function() {
     return this.get("companyzip");
  },
  setCompanyzip : function(v) {
     this.set({"companyzip" : v})
  },
  companycity  : function() {
     return this.get("companycity");
  },
  setCompanycity : function(v) {
     this.set({"companycity" : v})
  },
  companycountry  : function() {
     return this.get("companycountry");
  },
  setCompanycountry : function(v) {
     this.set({"companycountry" : v})
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
      , companyname :    !this.user().hasCompany() ? this.user().usercompanyname() : this.company().companyname()
      , companynumber :  !this.user().hasCompany() ? this.user().usercompanynumber()   : this.company().companynumber()
      , companyposition : this.user().companyposition()
      , companyaddress :  this.user().hasCompany() ? this.company().address()  : undefined
      , companyzip :      this.user().hasCompany() ? this.company().zip()  : undefined
      , companycity :     this.user().hasCompany() ? this.company().city()  : undefined
      , companycountry :  this.user().hasCompany() ? this.company().country()  : undefined
    }, {silent : true});
    this.trigger("reset");
  },
  createCompany : function(callback) {
     return new Submit({
      method : "POST",
      url : "/api/frontend/createcompany",
      ajax : true,
      ajaxsuccess : function(rs) {
        new FlashMessage({content: localization.account.accountDetails.companyCreated, color: "green"});
        if (callback!= undefined) callback();
      },
    }).send();
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
            new FlashMessage({content: localization.account.accountDetails.changeEmailMailSent1 + self.newemail() + localization.account.accountDetails.changeEmailMailSent2, color: "green"});
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
      companyname :  this.companyname(),
      companynumber :  this.companynumber(),
      companyposition : this.companyposition(),
      companyaddress :  this.companyaddress(),
      companyzip :      this.companyzip(),
      companycity :     this.companycity(),
      companycountry :  this.companycountry()
    }).send();
  },
  save : function() {
    var self  = this;
    this.updateProfile(function() {
      new FlashMessage({content : localization.account.accountDetails.detailSaved , color: "blue"});
      self.refresh();
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
      var model = this.model;
      var box = $("<div class='blue-box'/>");
      if (model.user().hasCompany())
        box.addClass("col");
      else
        box.addClass("blue-box");
      var header = $("<div class='account-header'/>").text(model.user().smartname());
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);


      var table = $("<table/>");
      body.append(table);

      var fstnameinput = $("<input type='text' name='fstname'/>").val(model.fstname());
      fstnameinput.change(function() {
          model.setFstname(fstnameinput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.fstname))).append($("<td/>").append(fstnameinput)));

      var sndnameinput = $("<input type='text' name='sndname'/>").val(model.sndname());
      sndnameinput.change(function() {
          model.setSndname(sndnameinput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.sndname))).append($("<td/>").append(sndnameinput)));

      var personnumberinput = $("<input type='text' name='personalnumber'/>").val(model.personnumber());
      personnumberinput.change(function() {
          model.setPersonnumber(personnumberinput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.personnumber))).append($("<td/>").append(personnumberinput)));

      var emailinput = $("<input type='text' disabled='disabled' style='width:140px;margin-right:10px'/>").val(model.email());
      emailinput.change(function() {
          model.setEmail(emailinput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.email))).append($("<td/>").append(emailinput).append(this.changeEmailButton())));

      var phoneinput = $("<input type='text' name='phone'/>").val(model.phone());
      phoneinput.change(function() {
          model.setPhone(phoneinput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.phone))).append($("<td/>").append(phoneinput)));

      if (!model.user().hasCompany()) {

          var companynameinput = $("<input type='text' name='companyname'/>").val(model.companyname());
          companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
            })
          table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyname))).append($("<td/>").append(companynameinput)));

          var companynumberinput = $("<input type='text' name='companynumber'/>").val(model.companynumber());
          companynumberinput.change(function() {
              model.setCompanynumber(companynumberinput.val());
            })
          table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companynumber))).append($("<td/>").append(companynumberinput)));

      };
      var companypositioninput = $("<input type='text' name='companyposition'/>").val(model.companyposition());
      companypositioninput.change(function() {
          model.setCompanyposition(companypositioninput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyposition))).append($("<td/>").append(companypositioninput)));

      return box;
    },
    companySettings : function() {
      // Building frame
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").text(model.company().companyname());
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);
      // Data table
            var table = jQuery("<table/>");

            var companynameinput = $("<input type='text'/>").val(model.companyname());
            if (!model.companyAdmin()) companynameinput.attr("disabled","disabled");
            companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
            })
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyname))).append($("<td/>").append(companynameinput)));

            var companynumberinput = $("<input type='text'/>").val(model.companynumber());
            if (!model.companyAdmin()) companynumberinput.attr("disabled","disabled");
            companynumberinput.change(function() {
              model.setCompanynumber(companynumberinput.val());
            })
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companynumber))).append($("<td/>").append(companynumberinput)));

            var companyaddressinput = $("<input type='text'/>").val(model.companyaddress());
            if (!model.companyAdmin()) companyaddressinput.attr("disabled","disabled");
            companyaddressinput.change(function() {
              model.setCompanyaddress(companyaddressinput.val());
            })
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyaddress))).append($("<td/>").append(companyaddressinput)));

            var companyzipinput = $("<input type='text'/>").val(model.companyzip());
            if (!model.companyAdmin()) companyzipinput.attr("disabled","disabled");
            companyzipinput.change(function() {
              model.setCompanyzip(companyzipinput.val());
            })
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companyzip))).append($("<td/>").append(companyzipinput)));

            var companycityinput = $("<input type='text'/>").val(model.companycity());
            if (!model.companyAdmin()) companycityinput.attr("disabled","disabled");
            companycityinput.change(function() {
              model.setCompanycity(companycityinput.val());
            })
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companycity))).append($("<td/>").append(companycityinput)));

            var companycountryinput = $("<input type='text'/>").val(model.companycountry());
            if (!model.companyAdmin()) companycountryinput.attr("disabled","disabled");
            companycountryinput.change(function() {
              model.setCompanycountry(companycountryinput.val());
            })
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.accountDetails.companycountry))).append($("<td/>").append(companycountryinput)));


            body.append(table);

      return box;
    },
    changeEmailButton : function() {
      var model = this.model;
      return Button.init({
        color: "blue",
        size: "tiny",
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


            var confirmation = Confirmation.popup({
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
              content: body
            });
            return false;
          }
      }).input();
    },
    createCompanyButton : function() {
      var model = this.model;
      return Button.init({
        color: "blue",
        size: "small",
        shape: "rounded",
        text: localization.account.accountDetails.createCompany,
        onClick: function() {
            mixpanel.track('Click create company button');
            var body = jQuery("<div class='account-body'>");
            body.append($("<p/>").text(localization.account.accountDetails.createCompanyExplanation));
            body.append($("<p/>").text(localization.account.accountDetails.createCompanyPreambule));

            var table = jQuery("<table/>");

            var companynameinput = $("<input type='text'/>").val(model.companyname());
            companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
            })
            table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companyname)).append($("<td/>").append(companynameinput)));

            var companynumberinput = $("<input type='text'/>").val(model.companynumber());
            companynumberinput.change(function() {
              model.setCompanynumber(companynumberinput.val());
            });
            table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companynumber)).append($("<td/>").append(companynumberinput)));

            var companyaddressinput = $("<input type='text'/>").val(model.companyaddress());
            companyaddressinput.change(function() {
              model.setCompanyaddress(companyaddressinput.val());
            })
            table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companyaddress)).append($("<td/>").append(companyaddressinput)));

            var companyzipinput = $("<input type='text'/>").val(model.companyzip());
            companyzipinput.change(function() {
              model.setCompanyzip(companyzipinput.val());
            })
            table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companyzip)).append($("<td/>").append(companyzipinput)));

            var companycityinput = $("<input type='text'/>").val(model.companycity());
            companynameinput.change(function() {
              model.setCompanycity(companycityinput.val());
            })
            table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companycity)).append($("<td/>").append(companycityinput)));

            var companycountryinput = $("<input type='text'/>").val(model.companycountry());
            companynameinput.change(function() {
              model.setCompanycountry(companycountryinput.val());
            })
            table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companycountry)).append($("<td/>").append(companycountryinput)));


            body.append(table);

            var confirmation = Confirmation.popup({
              onAccept: function() {
                  trackTimeout('Accept',
                               {'Accept' : 'create company'},
                                function() {
                                  model.createCompany(function() {
                                       model.updateProfile(function() {
                                         confirmation.close();
                                         model.refresh();
                                      });
                                    });
                                  });
              },
              onReject: function() {
                window.location = window.location
              },
              title: localization.account.accountDetails.createCompany,
              acceptButtonText: localization.account.accountDetails.createCompany,
              content: body
            });
            return false;
          }
        }).input();
    },
    saveButton : function() {
      var model = this.model;
      var button = Button.init({
        shape: "rounded",
        color : "blue",
        size: "small",
        cssClass : "save",
        text : localization.account.accountDetails.save,
        onClick : function() {
            mixpanel.track('Click save button');
          model.save();
          return false;
        }
      })
      return button.input();
    },
    render: function () {
       var model = this.model;
       if (!model.ready()) return;
       var container = $(this.el).empty();
       var box = $("<div class='tab-content account'/>");
       container.append(box);

       box.append(this.accountSettings());
       if (model.user().hasCompany())
        box.append(this.companySettings());
       var footerbox = $("<div class='account-footer'/>");
       box.append(footerbox);
       if (!model.user().hasCompany())
        footerbox.append(this.createCompanyButton().css("margin-right", "5px"));
       footerbox.append(this.saveButton());
       box.append("<div class='clearfix'></div>");
       return this;
    }
});


window.AccountSettings = function(args) {
          var model = new AccountSettingsModel(args);
          var view =  new AccountSettingsView({model : model, el : $("<div class='tab-container'/>")});
          return {
              refresh : function() {model.refresh();},
              el  : function() {return $(view.el);}
            };
};

})(window);
