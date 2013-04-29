/*
 * View for company branding. It contains two tabs - one with mail branding and one with email branding.
 */

(function(window){

window.CompanyBrandingModel = Backbone.Model.extend({
    initialize: function(args) {
      var self = this;
      if (!args.companyid) {
        this.submiturl = "/account/company";
        this.url = "/account/company/json";
        this._companyui = null;
        var user = new User();
        this.set({"user" : user})
        user.bind("change:ready",function() {
          self.reset();
          self.trigger('change:ready');
        });
        user.fetch({cache: false});
      } else {
        this.submiturl = "/adminonly/companyadmin/branding/"+args.companyid;
        this.url = "/adminonly/companyadmin/branding/json/"+args.companyid;
        this._companyui = new CompanyUI({url: this.url, companyid: args.companyid})
        this._companyui.bind("change:ready",function() {
          self.reset();
          self.trigger('change:ready');
        });
        this._companyui.fetch({cache: false});
      }
    },
    ready : function() {
        return (this.user() != undefined && this.user().ready()) || this._companyui != undefined;
    },
    refresh : function() {
      if (this.user() != undefined) {
          this.user().set({"ready" : false});
          this.user().fetch({cache: false});
      }
      else if (this._companyui != undefined) {
          this.user().set({"ready" : false});
          this._companyui.fetch({cache: false});
      }
      this.reset();
    },
    user : function() {
      return this.get("user");
    },
    companyui : function() {
      return this._companyui? this._companyui : this.user().company().companyui();
    },
    reset : function() {
      if (!this.ready()) return;
      var companybranding = this;
      var companyui = this.companyui();
      this.set({ "emailBranding" : new CompanyBrandingEmail({companyui : companyui})}, {silent: true});
      this.set({ "signviewBranding" : new CompanyBrandingSignView({companyui : companyui})}, {silent: true});
      this.set({ "editable" : companyui.editable() }, {silent: true});
      this.trigger("reset");
    },
    emailBranding: function() {
      return this.get("emailBranding");
    },
    signviewBranding: function() {
      return this.get("signviewBranding");
    },
    editable: function() {
      return this.get("editable");
    },
    save : function() {
          var self = this;
          LoadingDialog.open(localization.companyBranding.saveBranding);
          new Submit({
              method: "POST",
              url: self.submiturl,
              company: JSON.stringify(self.toJSON()),
              ajaxsuccess: function () {
                LoadingDialog.close();
              }
          }).sendAjax();
    },
    toJSON: function() {
      return ({
        id: this.get("id"),
        name: this.get("name"),
        address: this.get("address"),
        zip: this.get("zip"),
        city: this.get("city"),
        country: this.get("country"),
        companyemailfont: this.emailBranding().emailfont().customised() ? this.emailBranding().emailfont().font() : '',
        companyemailbordercolour: this.emailBranding().emailbordercolour().customised() ? this.emailBranding().emailbordercolour().colour() : '',
        companyemailbuttoncolour: this.emailBranding().emailbuttoncolour().customised() ? this.emailBranding().emailbuttoncolour().colour() : '',
        companyemailemailbackgroundcolour: this.emailBranding().emailemailbackgroundcolour().customised() ? this.emailBranding().emailemailbackgroundcolour().colour() : '',
        companyemailbackgroundcolour: this.emailBranding().emailbackgroundcolour().customised() ? this.emailBranding().emailbackgroundcolour().colour() : '',
        companyemailtextcolour: this.emailBranding().emailtextcolour().customised() ? this.emailBranding().emailtextcolour().colour() : '',
        companyemaillogo: this.emailBranding().emaillogo().customised() ? this.emailBranding().emaillogo().logo() : '',
        companysignviewtextcolour: this.signviewBranding().signviewtextcolour().customised() ? this.signviewBranding().signviewtextcolour().colour() : '',
        companysignviewtextfont: this.signviewBranding().signviewtextfont().customised() ? this.signviewBranding().signviewtextfont().font() : '',
        companysignviewbarscolour: this.signviewBranding().signviewbarscolour().customised() ? this.signviewBranding().signviewbarscolour().colour() : '',
        companysignviewbarstextcolour: this.signviewBranding().signviewbarstextcolour().customised() ? this.signviewBranding().signviewbarstextcolour().colour() : '',
        companysignviewbackgroundcolour: this.signviewBranding().signviewbackgroundcolour().customised() ? this.signviewBranding().signviewbackgroundcolour().colour() : '',
        companysignviewlogo: this.signviewBranding().signviewlogo().customised() ? this.signviewBranding().signviewlogo().logo() : ''
      });
    }
});

window.CompanyBrandingView = Backbone.View.extend({
  model: CompanyBrandingModel,
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("change:ready", this.render);
    this.model.bind("reset", this.render);
    this.render();
  },
  saveButton: function() {
    var model = this.model;
    return Button.init({
      color: "blue",
      shape: "rounded",
      size: "small",
      text: localization.companyBranding.saveBranding,
      onClick: function() {
          mixpanel.track('Click save branding button');
          model.save();
      }
    }).input();
  },
  render: function() {
    var model = this.model;

    if (!model.ready() || model.emailBranding() == undefined || model.signviewBranding() == undefined) {
      return this;
    }
    $(this.el).children().detach();
    var container = $("<div class='tab-content companybranding' />");
    $(this.el).append(container);
    var tabs = new KontraTabs({
            tabs: [
                new Tab({
                    name  : localization.companyBranding.emailBranding,
                    onActivate : function() {
                    },
                    elems : [model.emailBranding().el()]
                  }),
                new Tab({
                    name  : localization.companyBranding.signviewBranding,
                    onActivate : function() {
                    },
                    elems : [model.signviewBranding().el()]
                  })
                ]
        });
    container.append(tabs.el());

    container.append($("<div class='float-right save'/>").append(this.saveButton()));
    $(this.el).append("<div class='clearfix'></div>");
    return this;
  }
});

window.CompanyBranding = function(args) {
    var model = new CompanyBrandingModel(args);
    var view = new CompanyBrandingView({ model: model, el:$("<div class='tab-container account'/>") });
    return {
      refresh: function() {model.refresh();},
      el : function() { return $(view.el); }
    };
};

})(window);
