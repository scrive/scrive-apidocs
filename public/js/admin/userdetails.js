/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

(function(window){

var AdminUserDetailsModel = Backbone.Model.extend({
  initialize : function(args) {
    var self = this;
    var user = new User({id : args.userid, forAdmin : true});
    this.set({"user" : user});
    user.bind("change",function() {
      self.reset();
    });
    this.user().set({"ready" : false}, {silent: true});
    user.fetch({cache: false, processData: true});
    this.reset();
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
  accountType : function() {
    return this.get("accountType") || this.currentAccountType();
  },
  setAccountType : function(v) {
     this.set({"accountType" : v});
  },
  currentAccountType : function() {
     if (this.user().company() == undefined)
       return "privateaccount";
     else if (this.user().companyadmin())
       return "companyadminaccount";
     else
       return "companystandardaccount";
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
      , phone : this.user().phone()
      , lang :  this.user().lang() != "sv" ?  "en" : "sv"
      , companyname :    !this.user().hasCompany() ? this.user().usercompanyname() : this.company().companyname()
      , companynumber :  !this.user().hasCompany() ? this.user().usercompanynumber()   : this.company().companynumber()
      , companyposition : this.user().companyposition()
    }, {silent : true});
    this.trigger("reset");
  },
  saveDetails : function() {
    return new Submit({
        url : "/adminonly/useradmin/" + this.user().userid(),
        method : "POST",
        userfstname : this.fstname(),
        usersndname : this.sndname(),
        userpersonalnumber : this.personnumber(),
        userphone : this.phone(),
        useremail : this.email(),
        usercompanyname : this.companyname(),
        usercompanynumber : this.companynumber(),
        usercompanyposition : this.companyposition(),
        userlang :this.user().lang() != "sv" ?  "LANG_EN" : "LANG_SV",
        useraccounttype : this.get("accountType")
    });
  },
  resendInvitation : function() {
    return new Submit({
       url : "/adminonly/useradmin/sendinviteagain",
       method : "POST",
       userid : this.user().userid()
    });
  },
  refresh : function() {
    this.user().set({"ready" : false}, {silent: true});
    this.user().fetch({cache: false, processData: true});
    this.reset();

  }
});

var AdminUserDetailsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.render();
    },
    langSelect : function() {
      var self = this;
      var model = this.model;
      this.langselect = new Select({
                             name : model.lang() == "en" ? localization.account.accountSecurity.langEN : localization.account.accountSecurity.langSV,
                             onSelect : function(v) {model.setLang(v); self.langselect.el().replaceWith(self.langSelect().el()); return true;},
                             options:   model.lang() == "en" ? [{name: localization.account.accountSecurity.langSV, value: "sv"}] :
                                                                        [{name: localization.account.accountSecurity.langEN, value: "en"}],
                             textWidth : "203px",
                             optionsWidth : "230px"
                           });
      return this.langselect;
    },
    accountTypeName : function(name) {
      if (name == "privateaccount")
        return "Private account";
      else if (name == "companystandardaccount")
        return "Company account";
      else if (name == "companyadminaccount")
        return "Company admin";
    },
    accountTypeSelector : function() {
      var self = this;
      var model = this.model;
      this.accountTypeSelectorSelect = new Select({
        name : this.accountTypeName(model.accountType()),
        onSelect : function(v) {
          model.setAccountType(v);
          self.accountTypeSelectorSelect.replaceWith(self.accountTypeSelector());
          return true;
        },
        textWidth : "203px",
        optionsWidth : "230px",
        options : [  {name : this.accountTypeName("privateaccount"), value : "privateaccount"}
                   , {name : this.accountTypeName("companystandardaccount"), value : "companystandardaccount"}
                   , {name : this.accountTypeName("companyadminaccount"), value : "companyadminaccount"}
                  ]
      }).el();
      return this.accountTypeSelectorSelect;
    },
    accountDetails: function() {
      var self = this;
      var model = this.model;
      var box = $("<di/>");
      var table = $("<table style='border-collapse: separate; border-spacing: 10px;'/>");
      box.append(table);

      var fstnameinput = $("<input type='text' name='fstname'/>").val(model.fstname());
      fstnameinput.change(function() {
          model.setFstname(fstnameinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("First name"))).append($("<td/>").append(fstnameinput)));

      var sndnameinput = $("<input type='text' name='sndname'/>").val(model.sndname());
      sndnameinput.change(function() {
          model.setSndname(sndnameinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Last name"))).append($("<td/>").append(sndnameinput)));

      var personnumberinput = $("<input type='text' name='personalnumber'/>").val(model.personnumber());
      personnumberinput.change(function() {
          model.setPersonnumber(personnumberinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Personal number"))).append($("<td/>").append(personnumberinput)));

      var emailinput = $("<input type='text' name='email'/>").val(model.email());
      emailinput.change(function() {
          model.setEmail(emailinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Email"))).append($("<td/>").append(emailinput)));


      var phoneinput = $("<input type='text' name='phone'/>").val(model.phone());
      phoneinput.change(function() {
          model.setPhone(phoneinput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Phone"))).append($("<td/>").append(phoneinput)));


      if (!model.user().hasCompany()) {

          var companynameinput = $("<input type='text' name='companyname'/>").val(model.companyname());
          companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
            });
          table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company name"))).append($("<td/>").append(companynameinput)));

          var companynumberinput = $("<input type='text' name='companynumber'/>").val(model.companynumber());
          companynumberinput.change(function() {
              model.setCompanynumber(companynumberinput.val());
            });
          table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company number"))).append($("<td/>").append(companynumberinput)));

      }

      var companypositioninput = $("<input type='text' name='companyposition'/>").val(model.companyposition());
      companypositioninput.change(function() {
          model.setCompanyposition(companypositioninput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company position"))).append($("<td/>").append(companypositioninput)));

      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Language"))).append($("<td/>").append(this.langSelect().el())));

      if (model.user().hasCompany()) {
            table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company")))
                                   .append($("<td/>").append($("<a>Link</a>").attr("href","/adminonly/companyadmin/" + this.model.user().company().companyid()))));
      }

      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Account type"))).append($("<td/>").append(this.accountTypeSelector())));

      return box;
    },
    buttonsRow: function() {
      var model = this.model;
      var buttonRow = $("<div style='width:300px;height:50px;margin-top:30px;'/>");

      var saveButton = new Button({
                text: "Change details"
              , color: "green"
              , size: "tiny"
              , cssClass: "float-right"
              , onClick : function() {
                  model.saveDetails().sendAjax(function() {
                      new FlashMessage({color: "green", content : "Saved"});
                      model.refresh();
                  });
                }
          });

      var invitationButton = new Button({
                text: "Resend invitation"
              , color: "blue"
              , size: "tiny"
              , cssClass: "float-left"
              , onClick : function() {
                  model.resendInvitation().sendAjax(function() {
                      new FlashMessage({color: "green", content : "Invitation send"});
                      model.refresh();
                  });
                }
          });
      return buttonRow.append(saveButton.el()).append(invitationButton.el());

    },
    render: function () {
       var self = this;
       var model = this.model;
       var container = $(this.el);
       if (!model.ready()) return;
       container.empty();
       container.append(this.accountDetails()).append(this.buttonsRow());
    }
});


window.AdminUserDetails = function(args) {
          var model = new AdminUserDetailsModel(args);
          var view =  new AdminUserDetailsView({model : model, el : $("<div class='tab-container account'/>")});
          this.el = function() {return $(view.el);};
          this.refresh = function() {
              model.refresh();
          };
};

})(window);
