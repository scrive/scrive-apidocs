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
      self.reset();
    });
    user.fetch();
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
      , phone : this.user().phone()
      , companyname :    !this.user().hasCompany() ? this.user().usercompanyname() : this.company().companyname()
      , companynumber :  !this.user().hasCompany() ? this.user().companynumber()   : this.company().companynumber()
      , companyposition : this.user().companyposition()
      , companyaddress :  this.user().hasCompany() ? this.company().adress()  : undefined
      , companyzip :      this.user().hasCompany() ? this.company().zip()  : undefined
      , companycity :     this.user().hasCompany() ? this.company().city()  : undefined
      , companycountry :  this.user().hasCompany() ? this.company().county()  : undefined
    }, {silent : true});
    this.trigger("reset");
  },
  save : function() {
    var self = this;
    var submit = new Submit({
      method : "POST",
      url : "/account",
    }).send();
  },
  refresh : function() {    this.user().fetch(); this.reset(); }
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
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2>").text(model.user().smartname())).append("<div class='image'/>");
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);


      var table = $("<table/>");
      body.append(table);

      var fstnameinput = $("<input type='text'/>").val(model.fstname());
      fstnameinput.change(function() {
          model.setFstname(fstnameinput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.fstname + ":")).append($("<td/>").append(fstnameinput)));

      var sndnameinput = $("<input type='text'/>").val(model.sndname());
      sndnameinput.change(function() {
          model.setSndname(sndnameinput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.sndname + ":")).append($("<td/>").append(sndnameinput)));

      var personnumberinput = $("<input type='text'/>").val(model.personnumber());
      personnumberinput.change(function() {
          model.setPersonnumber(personnumberinput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.personnumber + ":")).append($("<td/>").append(personnumberinput)));      

      var emailinput = $("<input type='text'/>").val(model.email());
      emailinput.change(function() {
          model.setEmail(emailinput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.email + ":")).append($("<td/>").append(emailinput)));

      var phoneinput = $("<input type='text'/>").val(model.phone());
      phoneinput.change(function() {
          model.setPhone(phoneinput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.phone + ":")).append($("<td/>").append(phoneinput)));

      if (!model.user().hasCompany()) {
        
          var companynameinput = $("<input type='text'/>").val(model.companyname());
          companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
            })
          table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companyname + ":")).append($("<td/>").append(companynameinput)));

          var companynumberinput = $("<input type='text'/>").val(model.companynumber());
          companynumberinput.change(function() {
              model.setCompanyname(companynumberinput.val());
            })
          table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companynumber + ":")).append($("<td/>").append(companynumberinput)));
          
      };
      var companypositioninput = $("<input type='text'/>").val(model.companyposition());
      companypositioninput.change(function() {
          model.setCompanyposition(companypositioninput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountDetails.companyposition+ ":" )).append($("<td/>").append(companypositioninput)));
      
      return box;
    },
    companySettings : function() {
      // Building frame
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2>").text(model.company().companyname())).append("<div class='image'/>");
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);


      return box;
    },
    saveButton : function() {
      var model = this.model;
      var box = $("<div class='account-footer'/>");
      var button = Button.init({
        color : "green",
        size: "small",
        text : localization.account.accountDetails.save,
        onClick : function() {
          model.save();
          return false;
        }
      }) 
      return box.append(button.input());
    },
    render: function () {
       var model = this.model;
       if (!model.ready()) return;
       var container = $(this.el).empty();
       var box = $("<div class='tab-content account'/>");
       container.append(box);
       
       box.append(this.accountSettings());
       box.append(this.companySettings());
       box.append(this.saveButton());
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
