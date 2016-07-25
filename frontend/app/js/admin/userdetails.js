var Backbone = require("backbone");
var React = require("react");
var Select = require("../../scripts/common/select");
var User = require("../account/user.js").User;
var Submit = require("../submits.js").Submit;
var _ = require("underscore");
var $ = require("jquery");
var Button = require("../buttons.js").Button;
var FlashMessage = require("../flashmessages.js").FlashMessage;
var InfoTextInput = require("../infotextinputs.js").InfoTextInput;
var NumberValidation = require("../validation.js").NumberValidation;
var Confirmation = require("../confirmations.js").Confirmation;

/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */


var AdminUserDetailsModel = Backbone.Model.extend({
  defaults : {
    newcompanyid : ""
  },
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
  accountType : function() {
    return this.get("accountType") || this.currentAccountType();
  },
  setAccountType : function(v) {
     this.set({"accountType" : v});
  },
  currentAccountType : function() {
     if (this.user().companyadmin())
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
  newcompanyid : function() {
    return this.get("newcompanyid");
  },
  setNewcompanyid : function(v) {
    this.set({"newcompanyid" : v});
  },
  reset : function() {
    if (!this.ready()) return;
    this.set({
        fstname : this.user().fstname()
      , sndname : this.user().sndname()
      , personnumber : this.user().personalnumber()
      , email : this.user().email()
      , phone : this.user().phone()
      , lang :  this.user().lang()
      , companyposition : this.user().companyposition()
    }, {silent : true});
    this.trigger("reset");
  },
  saveDetails : function(callback) {
    return new Submit({
        url : "/adminonly/useradmin/" + this.user().userid(),
        method : "POST",
        ajaxsuccess : callback,
        userfstname : this.fstname(),
        usersndname : this.sndname(),
        userpersonalnumber : this.personnumber(),
        userphone : this.phone(),
        useremail : this.email(),
        usercompanyposition : this.companyposition(),
        userlang : this.lang(),
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
  moveToDifferentCompany : function() {
    return new Submit({
       url : "/adminonly/useradmin/move/" + this.user().userid(),
       method : "POST",
       companyid : this.newcompanyid()
    });
  },
  deleteUser : function() {
    return new Submit({
       url : "/adminonly/useradmin/delete/" + this.user().userid(),
       method : "POST"
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
        this.model.bind('change:lang', this.render);
        this.model.bind('change:accountType', this.render);
        this.render();
    },
    langSelect : function() {
      var self = this;
      var model = this.model;

      var languages = [
          {name: "Swedish", value: "sv"}
        , {name: "English", value: "en"}
        , {name: "German", value: "de"}
        , {name: "French", value: "fr"}
        , {name: "Dutch", value: "nl"}
        , {name: "Italian", value: "it"}
        , {name: "Norwegian", value: "no"}
        , {name: "Portuguese", value: "pt"}
        , {name: "Spanish", value: "es"}
        , {name: "Danish", value: "da"}
        , {name: "Greek", value: "el"}
        , {name: "Finnish", value: "fi"}
        , {name: "Icelandic", value: "is"}
        , {name: "Estonian", value: "et"}
        , {name: "Latvian", value: "lv"}
        , {name: "Lithuanian", value: "lt"}
      ];

      languages = _.sortBy(languages, function(l) {return l.name.toLowerCase();});

      var $select = $("<span>");

      React.render(React.createElement(Select, {
        isOptionSelected: function(o) {
          return o.value == model.lang();
        },
        onSelect : function(v) {model.setLang(v); return true;},
        options: languages,
        textWidth : 240
      }), $select[0]);

      return $select;
    },
    accountTypeName : function(name) {
      if (name == "companystandardaccount")
        return "Company account";
      else if (name == "companyadminaccount")
        return "Company admin";
    },
    accountTypeSelector : function() {
      var self = this;
      var model = this.model;
      var $select = $("<span>");

      React.render(React.createElement(Select, {
        isOptionSelected: function(o) {
          return o.value == model.accountType();
        },
        onSelect : function(v) {
          model.setAccountType(v);
        },
        textWidth : 240,
        options : [  {name : this.accountTypeName("companystandardaccount"), value : "companystandardaccount"}
                   , {name : this.accountTypeName("companyadminaccount"), value : "companyadminaccount"}
                  ]
      }), $select[0]);

      return $select;
    },
    accountDetails: function() {
      var self = this;
      var model = this.model;
      var box = $("<div/>");
      var table = $("<table style='border-collapse: separate; border-spacing: 10px;'/>");
      box.append(table);

      var idinput = $("<input type='text' readonly='' style='color:#666666'/>").val(model.user().userid());
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("User ID"))).append($("<td/>").append(idinput)));

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


      var companypositioninput = $("<input type='text' name='companyposition'/>").val(model.companyposition());
      companypositioninput.change(function() {
          model.setCompanyposition(companypositioninput.val());
        });
      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company position"))).append($("<td/>").append(companypositioninput)));

      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Language"))).append($("<td/>").append(this.langSelect())));

      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Company")))
                                   .append($("<td/>").append($("<a>Link to company </a>").append($("<span>").text(this.model.user().company().companyname()))
                                                                .attr("href","/adminonly/companyadmin/" + this.model.user().company().companyid()))));

      table.append($("<tr/>").append($("<td/>").append($("<label/>").text("Account type"))).append($("<td/>").append(this.accountTypeSelector())));

      return box;
    },
    buttonsRow: function() {
      var self = this;
      var model = this.model;
      var buttonRow = $("<div style='width:600px;height:50px;margin-top:30px;'/>");


      var deleteButton = new Button({
                text: "Delete user"
              , type: "cancel"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  self.openDeleteModal();
                }
          });

      var invitationButton = new Button({
                text: "Resend invitation"
              , type: "optional"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  model.resendInvitation().sendAjax(function() {
                      new FlashMessage({type: 'success', content : "Invitation send"});
                      model.refresh();
                  });
                }
          });

      var moveButton = new Button({
                text: "Move to different company"
              , type: "optional"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  self.openMoveToDifferentCompanyModal();
                }
      });

      var saveButton = new Button({
                text: "Change details"
              , type: "action"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  model.saveDetails(function (resp) {
                    if (resp.changed) {
                      new FlashMessage({type: 'success', content: "Saved"});
                      model.refresh();
                    } else {
                      new FlashMessage({type: 'error', content: "Failure. User already exists"});
                    }
                  }).sendAjax();
                }
          });

      return buttonRow.append(deleteButton.el()).append(invitationButton.el()).append(moveButton.el()).append(saveButton.el());

    },
    openMoveToDifferentCompanyModal : function() {
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
      var popup = new Confirmation({
        title : "Move user to different company",
        acceptText: "Move",
        content : label,
        onAccept : function() {
          model.moveToDifferentCompany().sendAjax(
            function() {
                new FlashMessage({type: 'success', content : "Moved"});
                model.refresh();
                popup.close();
                return false;
            },
            function() {
              new FlashMessage({type: 'error', content : "Failed"});
              return false;
            }
         );
        }
      });
    },
    openDeleteModal : function() {
      var model = this.model;
      new Confirmation({
        title : "Delete",
        acceptText: "Delete",
        content : $("<div style='text-align:center;'>Are you sure that you want to delete this user?</div>"),
        onAccept : function() {
          model.deleteUser().sendAjax(
            function() {
                window.location = "/adminonly/companyadmin/" + model.user().company().companyid() + "/#users";
                return false;
            },
            function() {
              new FlashMessage({type: 'error', content : "Failed"});
              return false;
            }
         );
        }
      });
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


var AdminUserDetails = exports.AdminUserDetails = function(args) {
          var model = new AdminUserDetailsModel(args);
          var view =  new AdminUserDetailsView({model : model, el : $("<div class='tab-container account'/>")});
          this.el = function() {return $(view.el);};
          this.refresh = function() {
              model.refresh();
          };
};

