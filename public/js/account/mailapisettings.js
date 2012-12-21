/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

var MailAPISettingsModel = Backbone.Model.extend({
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
    user.fetch({cache: false});
    this.reset();
  },
  user : function() {
     return this.get("user");
  },
  ready : function() {
    return this.user().ready();
  },
  enablePersonal : function() {
    return this.get("enablePersonal");
  },
  setEnablePersonal : function(v) {
    this.set({"enablePersonal" : v} );
  },
  resetKeys : function() {
    return this.get("resetKeys");
  },
  setResetKeys : function(v) {
    this.set({"resetKeys" : v} );
  },
  resetLimit : function() {
    return this.get("resetLimit");
  },
  setResetLimit : function(v) {
    this.set({"resetLimit" : v} );
  },
  dailyLimitChange : function() {
    return this.get("dailyLimitChange");
  },
  setDailyLimitChange : function(v) {
    this.set({"dailyLimitChange" : v} );
  },
  hasPersonalMailApi : function() {
      return this.user().hasMailApi();
  },
  hasCompanyMailApi : function() {
      return this.user().hasCompany() && this.user().company().hasMailApi();
  },
  personalMailApi : function() {
      return this.user().mailapi();
  },
  companyMailApi : function() {
      return this.user().company().mailapi();
  },
  reset : function() {
    if (!this.ready()) return;
    this.set({
      enablePersonal : this.user().hasMailApi(),
      resetKeys : false,
      resetLimit : false,
      dailyLimitChange : this.user().hasMailApi() ? this.user().mailapi().limit() : undefined
    }, {silent : true});
    this.trigger("reset");
  },
  save : function() {
    var self = this;
    var submit = new Submit({
      method : "POST",
      url : "/account/mailapi",
      api_enabled : this.enablePersonal() == true ? "on" : undefined,
      reset_key : this.resetKeys() == true ? "on" : undefined,
      reset_senttoday : this.resetLimit() == true ? "on" : undefined,
      daily_limit : this.dailyLimitChange()
    }).sendAjax(function() {
        window.location.reload();
    });
  },
  refresh : function() {    this.user().fetch({cache: false}); }
});


  
var MailAPISettingsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind("reset", this.render);
        this.render();
    },
    personalSettings : function() {
      // Building frame
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2/>").text(localization.account.mailAPI.personalMailAPIHeader))
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      var disablecheckbox = $("<input type='checkbox' autocomplete='false'/>");
      if (model.hasPersonalMailApi()) disablecheckbox.attr("checked","checked");
      disablecheckbox.change(function() {
          model.setEnablePersonal(disablecheckbox.is(":checked"));
          return true;
        });
      var disablelabel = $("<label/>").text(localization.account.mailAPI.enablePersonal);
      body.append(disablecheckbox).append(disablelabel);

      if (model.hasPersonalMailApi()) {
        // Filling content
        var personalMailApi = this.model.personalMailApi();
        body.append($("<label/>").text(localization.account.mailAPI.activeMails));
        var list = $("<ul class='mailapiemailslist'/>");
        var a1 = $("<a/>").attr('href',"mailto:contract+"+personalMailApi.key()+"@api.scrive.com").text("contract+"+personalMailApi.key()+"@api.scrive.com");
        var a2 = $("<a/>").attr('href',"mailto:offer+"+personalMailApi.key()+"@api.scrive.com").text("offer+"+personalMailApi.key()+"@api.scrive.com");
        var a3 = $("<a/>").attr('href',"mailto:order+"+personalMailApi.key()+"@api.scrive.com").text("order+"+personalMailApi.key()+"@api.scrive.com");
        list.append($("<li/>").append(a1));
        list.append($("<li/>").append(a2));
        list.append($("<li/>").append(a3));
        body.append(list)

        var resetcheckbox = $("<input type='checkbox' autocomplete='false'/>");
        resetcheckbox.change(function() {
          model.setResetKeys(resetcheckbox.is(":checked"));
          return true;
        });
        var resetlabel = $("<label/>").text(localization.account.mailAPI.resetKeys);
        body.append(resetcheckbox).append(resetlabel);
      }
        
      return box;
    },
    limits: function() {
      var box = $("<div class='col'/>");
      var model = this.model;
      // Building frame
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2/>").text(localization.account.mailAPI.limitMailAPIHeader))
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      var table = $("<table class='mailapitable'/>");
      body.append(table);
      
      var limitinput = $("<input type='text'/>").val(model.hasPersonalMailApi() ? this.model.dailyLimitChange() : "N/A");
      if (!model.hasPersonalMailApi())
        limitinput.attr("disabled", "disabled");
      else
        limitinput.change(function() {
          model.setDailyLimitChange(limitinput.val());
        })
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.mailAPI.dailyLimit))).append($("<td/>").append(limitinput)));

      var sendinput = $("<input type='text' disabled='disabled'/>").val(model.hasPersonalMailApi() ? this.model.personalMailApi().sent() : "N/A");
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text(localization.account.mailAPI.processedToday))).append($("<td/>").append(sendinput)));

      if (model.hasPersonalMailApi()) {
        var resetbox = $("<div class='mailapi'/>");
        var checkbox = $("<input type='checkbox' autocomplete='false'/>");
        checkbox.change(function() {
          model.setResetLimit(checkbox.is(":checked"));
          return true;
        });
        var label = $("<label/>").append($("<label/>").text(localization.account.mailAPI.resetLimit));
        body.append(resetbox.append(checkbox).append(label));
      }
           
      return box;
    },
    companySettings : function() {
      var companyMailApi = this.model.companyMailApi();
      // Building frame
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2/>").text(localization.account.mailAPI.companyMailAPIHeader))
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      // Filling content
      body.append($("<label/>").text(localization.account.mailAPI.activeMails));
      var list = $("<ul class='mailapiemailslist'/>");
      var a1 = $("<a/>").attr('href',"mailto:contract+"+companyMailApi.key()+"@api.scrive.com").text("contract+"+companyMailApi.key()+"@api.scrive.com");
      var a2 = $("<a/>").attr('href',"mailto:offer+"+companyMailApi.key()+"@api.scrive.com").text("offer+"+companyMailApi.key()+"@api.scrive.com");
      var a3 = $("<a/>").attr('href',"mailto:order+"+companyMailApi.key()+"@api.scrive.com").text("order+"+companyMailApi.key()+"@api.scrive.com");
      list.append($("<li/>").append(a1));
      list.append($("<li/>").append(a2));
      list.append($("<li/>").append(a3));
      body.append(list)
      body.append("<BR/>");
      body.append($("<label/>").text(localization.account.mailAPI.dailyLimit + ":" + companyMailApi.limit()));
      body.append("<BR/>");
      body.append($("<label/>").text(localization.account.mailAPI.processedToday + ":" + companyMailApi.sent()));
      body.append("<BR/>");
      
      return box;
    },
    saveButton : function() {
      var model = this.model;
      var box = $("<div class='account-footer'/>");
      var button = Button.init({
        color : "green",
        size: "small",
        text : localization.account.mailAPI.save,
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
       
       box.append(this.personalSettings());
       box.append(this.limits());
       if (model.hasCompanyMailApi())
          box.append(this.companySettings());
       box.append(this.saveButton());
       box.append("<div class='clearfix'></div>");
       return this;
    }
});


window.MailAPISettings = function(args) {
          var model = new MailAPISettingsModel();
          var view =  new MailAPISettingsView({model : model, el : $("<div class='tab-container'/>")});
          return {
              refresh : function() {model.user().fetch({cache: false}); model.trigger("reset");},
              el  : function() {return $(view.el);}
            };
};

})(window);
