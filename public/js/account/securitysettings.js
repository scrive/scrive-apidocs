/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

var SecuritySettingsModel = Backbone.Model.extend({
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
  oldpassword : function() {
     return this.get("oldpassword");
  },
  password1 : function() {
     return this.get("password1");
  },
  password2 : function() {
     return this.get("password2");
  },
  useFooter : function() {
    return this.get("useFooter");
  },
  footer : function() {
    return this.get("footer");
  },
  region : function() {
    return this.get("region");
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
  setUseFooter : function(v) {
    this.set({"useFooter" : v} );
  },
  setFooter : function(v) {
    this.set({"footer" : v} );
  },
  setRegion : function(v) {
    this.set({"region" : v}, {silent : true} );
    this.trigger("change:region");
  },
  reset : function() {
    if (!this.ready()) return;
    this.set({
      oldpassword : "",
      password1  : "",
      password2  : "",
      footer     : this.user().footer() ,
      region     : this.user().region() != "se" ?  "REGION_GB" : "REGION_SE",
      useFooter  : this.user().footer() != undefined 
    }, {silent : true});
    this.trigger("reset");
  },
  save : function() {
    var self = this;
    var submit = new Submit({
      method : "POST",
      url : "/account/security",
      oldpassword : this.oldpassword(),
      password  : this.password1(),
      password2  : this.password2(),
      region     : this.region(),
      footerCheckbox : this.useFooter() ? "on" : undefined,
      customfooter  : this.useFooter() ? this.footer() : undefined
    }).send();
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
      var header = $("<div class='account-header'/>").append($("<h2>").text(localization.account.accountSecurity.passwordSection))
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      var table = $("<table/>");
      body.append(table);

      var oldpasswordinput = $("<input type='password' autocomplete='off'/>");
      oldpasswordinput.change(function() {
          model.setOldPassword(oldpasswordinput.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountSecurity.oldpassword)).append($("<td/>").append(oldpasswordinput)));

      var password1input = $("<input type='password' autocomplete='off'/>");
      password1input.change(function() {
          model.setPassword1(password1input.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountSecurity.newpassword1)).append($("<td/>").append(password1input)));

      var password2input = $("<input type='password' autocomplete='off'/>");
      password2input.change(function() {
          model.setPassword2(password2input.val());
        })
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountSecurity.newpassword2)).append($("<td/>").append(password2input)));
        
      return box;
    },
    regionSettings : function() {
      // Building frame
      var self = this;
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2/>").text(localization.account.accountSecurity.regionSection))
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);

      var table = $("<table/>");
      body.append(table);

      this.regionSelectBox = $("<td/>");
      var updateRegionSelect = function() {
         if (self.regionSelect != undefined)  self.regionSelect.clear();
         self.regionSelectBox.empty();
         self.regionSelect = new Select({
                             textWidth : "90px",
                             name : model.region() == "REGION_GB" ? localization.account.accountSecurity.regionGB : localization.account.accountSecurity.regionSE,
                             onSelect : function(v) {model.setRegion(v);return true;},
                             options:   model.region() == "REGION_GB" ? [{name: localization.account.accountSecurity.regionSE, value: "REGION_SE"}] :
                                                                        [{name: localization.account.accountSecurity.regionGB, value: "REGION_GB"}]
                           });
         self.regionSelectBox.append(self.regionSelect.view().el)
      };
      updateRegionSelect();
      model.bind("change:region",updateRegionSelect);
      table.append($("<tr/>").append($("<td/>").text(localization.account.accountSecurity.region)).append(this.regionSelectBox));
      return box;
    },
    footerSettings : function() {
      // Building frame
      var self = this;
      var model = this.model;
      var box = $("<div class='col'/>");
      var header = $("<div class='account-header'/>").append($("<h2/>").text(localization.account.accountSecurity.footerSection))
      var body = $("<div class='account-body'/>");
      box.append(header).append(body);
      
      var checkbox = $("<input type='checkbox' autocomplete='false'/>");
      if (model.useFooter()) checkbox.attr("checked","checked");
      checkbox.change(function() {
          model.setUseFooter(checkbox.is(":checked"));
      });
                                                
      var label = $("<label style='margin-left: 10px'/>").text(localization.account.accountSecurity.useFooter);
      body.append($("<div class=''/>").append(checkbox).append(label));

      var cfb = $("<div class='customfooterbox'/>");
      var updateTinyVisibility = function() {
          if (!model.useFooter())
            cfb.css("display","none");
          else
            cfb.css("display","block");
      }
      updateTinyVisibility();
      model.bind("change:useFooter", updateTinyVisibility);
      
      var cf = $("<textarea id='customfooter' name='customfooter' style='width:350px;height:110px'/>").html(model.useFooter() ? model.footer() : "");
      setTimeout(function() {cf.tinymce({
                                script_url: '/tiny_mce/tiny_mce.js',
                                theme: "advanced",
                                theme_advanced_toolbar_location: "top",
                                theme_advanced_buttons1: "bold,italic,underline",
                                theme_advanced_buttons2: "",
                                convert_urls: false,
                                theme_advanced_toolbar_align: "left",
                                valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul",
                                onchange_callback  : function (inst) {
                                  model.setFooter(inst.getBody().innerHTML);
                                }
                        });}, 100);
      
      body.append(cfb.append(cf));
      
      return box;
    },
    saveButton : function() {
      var model = this.model;
      var box = $("<div class='account-footer'/>");
      var button = Button.init({
        color : "green",
        size: "small",
        text : localization.account.accountSecurity.save,
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
       
       box.append(this.passwordSettings());
       box.append(this.regionSettings());
       box.append(this.footerSettings());
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
