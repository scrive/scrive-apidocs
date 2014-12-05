/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

define(['Backbone', 'legacy_code'], function() {

var AdminCompanyDetailsModel = Backbone.Model.extend({
  initialize : function(args) {
    var self = this;
    var company = new Company({companyid : args.companyid, forAdmin : true});
    this.set({"company" : company});
    company.bind("change",function() {
      self.reset();
    });
    this.company().set({"ready" : false}, {silent: true});
    company.fetch({cache: false, processData: true});
    this.reset();
  },
  company : function() {
     return this.get("company");
  },
  ready : function() {
     return this.company().ready();
  },
  companyid : function() {
     return this.get("companyid");
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
  companycgidisplayname: function() {
     return this.get("companycgidisplayname");
  },
  setCompanycgidisplayname : function(v) {
     this.set({"companycgidisplayname" : v});
  },
  companyidledoctimeout: function() {
     return this.get("companyidledoctimeout");
  },
  setCompanyidledoctimeout : function(v) {
     this.set({"companyidledoctimeout" : v});
  },
  companyipaddressmasklist: function() {
     return this.get("companyipaddressmasklist");
  },
  setCompanyipaddressmasklist : function(v) {
     this.set({"companyipaddressmasklist" : v});
  },
  companyallowsavesafetycopy: function() {
    return this.get("companyallowsavesafetycopy");
  },
  setCompanyallowsavesafetycopy: function(v) {
    return this.set({"companyallowsavesafetycopy":v});
  },
  newcompanyid : function() {
    return this.get("newcompanyid");
  },
  setNewcompanyid : function(v) {
    this.set({"newcompanyid" : v});
  },
  reset : function() {
    if (!this.ready()) return;
    this.set({
        companyname :     this.company().companyname()
      , companynumber :   this.company().companynumber()
      , companyaddress :  this.company().address()
      , companyzip :      this.company().zip()
      , companycity :     this.company().city()
      , companycountry :  this.company().country()
      , companyipaddressmasklist : this.company().ipaddressmasklist()
      , companysmsoriginator : this.company().smsoriginator()
      , companycgidisplayname : this.company().cgidisplayname()
      , companyidledoctimeout : this.company().idledoctimeout()
      , companyallowsavesafetycopy : this.company().allowsavesafetycopy()
    }, {silent : true});
    this.trigger("reset");
  },
  saveDetails : function() {
    return new Submit({
        url : "/adminonly/companyadmin/" + this.company().companyid(),
        method : "POST",
        companyname : this.companyname(),
        companynumber : this.companynumber(),
        companyaddress : this.companyaddress(),
        companyzip : this.companyzip(),
        companycity : this.companycity(),
        companycountry : this.companycountry(),
        companyipaddressmasklist : this.companyipaddressmasklist(),
        companysmsoriginator : this.companysmsoriginator(),
        companycgidisplayname : this.companycgidisplayname(),
        companyidledoctimeout : this.companyidledoctimeout(),
        companyallowsavesafetycopy : this.companyallowsavesafetycopy()
    });
  },
  mergeToDifferentCompany : function() {
    return new Submit({
       url : "/adminonly/companyadmin/merge/" + this.companyid(),
       method : "POST",
       companyid : this.newcompanyid()
    });
  },
  refresh : function() {
    this.company().set({"ready" : false}, {silent: true});
    this.company().fetch({cache: false, processData: true});
    this.reset();

  }
});

var AdminCompanyDetailsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.render();
    },
    accountDetails: function() {
      var self = this;
      var model = this.model;
      var box = $("<div/>");
      var table = $("<table style='border-collapse: separate; border-spacing: 10px;'/>");
      box.append(table);

      var idinput = $("<input type='text' readonly='' style='color:#666666'/>").val(model.company().companyid());
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company ID"))).append($("<td/>").append(idinput)));

      var companynameinput = $("<input type='text'/>").val(model.companyname());
      companynameinput.change(function() {
              model.setCompanyname(companynameinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Name"))).append($("<td/>").append(companynameinput)));

      var companynumberinput = $("<input type='text'/>").val(model.companynumber());
      companynumberinput.change(function() {
              model.setCompanynumber(companynumberinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Number"))).append($("<td/>").append(companynumberinput)));

      var companyaddressinput = $("<input type='text'/>").val(model.companyaddress());
      companyaddressinput.change(function() {
              model.setCompanyaddress(companyaddressinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Adress"))).append($("<td/>").append(companyaddressinput)));

      var companyzipinput = $("<input type='text'/>").val(model.companyzip());
      companyzipinput.change(function() {
              model.setCompanyzip(companyzipinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Zip"))).append($("<td/>").append(companyzipinput)));

      var companycityinput = $("<input type='text'/>").val(model.companycity());
      companycityinput.change(function() {
              model.setCompanycity(companycityinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("City"))).append($("<td/>").append(companycityinput)));

      var companycountryinput = $("<input type='text'/>").val(model.companycountry());
      companycountryinput.change(function() {
              model.setCompanycountry(companycountryinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Country"))).append($("<td/>").append(companycountryinput)));

      var companyipaddressmasklistinput = $("<input type='text'/>").val(model.companyipaddressmasklist());
      companyipaddressmasklistinput.change(function() {
              model.setCompanyipaddressmasklist(companyipaddressmasklistinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("IP address mask"))).append($("<td/>").append(companyipaddressmasklistinput)));

      var companysmsoriginatorinput = $("<input type='text' maxlength=11/>").val(model.companysmsoriginator());
      companysmsoriginatorinput.change(function() {
              model.setCompanysmsoriginator(companysmsoriginatorinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("SMS originator"))).append($("<td/>").append(companysmsoriginatorinput)));

      var companycgidisplaynameinput = $("<input type='text' maxlength=30/>").val(model.companycgidisplayname());
      companycgidisplaynameinput.change(function() {
              model.setCompanycgidisplayname(companycgidisplaynameinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("CGI display name (BankID only)"))).append($("<td/>").append(companycgidisplaynameinput)).append($("<td/>").text("This has to be accepted by CGI. Else BanID will not work.")));


      var companyidledoctimeoutinput = $("<input type='number' min='"+model.company().minidledoctimeout()+"' max='"+model.company().maxidledoctimeout()+"'/>").val(model.companyidledoctimeout());
      companyidledoctimeoutinput.change(function() {
              model.setCompanyidledoctimeout(companyidledoctimeoutinput.val());
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Move idle documents to trash after days"))).append($("<td/>").append(companyidledoctimeoutinput)).append($("<td/>").text("Applies to all documents except pending documents and templates. If empty, documents will not be moved. Available values: "+model.company().minidledoctimeout()+" to "+model.company().maxidledoctimeout()+".")));

      var companyallowsavesafetycopyinput = $("<input type='checkbox'/>").attr("checked",model.companyallowsavesafetycopy());
      companyallowsavesafetycopyinput.change(function() {
              model.setCompanyallowsavesafetycopy(companyallowsavesafetycopyinput.is(":checked"));
      });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Allow to save document"))).append($("<td/>").append(companyallowsavesafetycopyinput)).append($("<td/>").text("Users might be offered to save documents after they have signed.")));

      return box;
    },
    openMergeToDifferentCompanyModal : function() {
      var model = this.model;
      var nameBox = $("<div style='color:#666666;margin-left:10px;font-size:10px;width:200px;display:inline'>");
      var input = new InfoTextInput({
                    infotext : "ID",
                    value: model.newcompanyid(),
                    onChange : function(v) {
                      model.setNewcompanyid(v);
                      if (new NumberValidation().validateData(v)) {
                        new Submit({
                          url: "/adminonly/companyadmin/details/"+ v,
                          expectedType: "json",
                          ajaxsuccess: function(resp) {
                            nameBox.text("Company with name: " + resp.companyname);
                          },
                          ajaxerror : function() {
                            nameBox.text("No company is matching given id");
                          }
                        }).sendAjax();
                      }
                      else
                       nameBox.text("Company id must contains only numbers");
                    }
                  });
      var label = $("<label> Company ID: <label>").append(input.el()).append(nameBox);
      new Confirmation({
        title : "Merge this company to different company",
        acceptText: "Merge",
        content : label,
        onAccept : function() {
          model.mergeToDifferentCompany().sendAjax(
            function() {
                new FlashMessage({color: "green", content : "Merged"});
                window.location = "/adminonly/companyadmin/" + model.newcompanyid();
                return false;
            },
            function() {
              new FlashMessage({color: "red", content : "Failed"});
              return false;
            }
         );
        }
      });
    },
    buttonsRow: function() {
      var self = this;
      var model = this.model;
      var buttonRow = $("<div style='width:500px;height:50px;margin-top:30px;'/>");


      var mergeButton = new Button({
                text: "Merge to different company"
              , color: "blue"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  self.openMergeToDifferentCompanyModal();
                }
      });

      var saveButton = new Button({
                text: "Change details"
              , color: "green"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  model.saveDetails().sendAjax(function() {
                      new FlashMessage({color: "green", content : "Saved"});
                      model.refresh();
                  });
                }
          });
      return buttonRow.append(mergeButton.el()).append(saveButton.el());
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


window.AdminCompanyDetails = function(args) {
          var model = new AdminCompanyDetailsModel(args);
          var view =  new AdminCompanyDetailsView({model : model, el : $("<div class='tab-container account'/>")});
          this.el = function() {return $(view.el);};
          this.refresh = function() {
              model.refresh();
          };
};

});
