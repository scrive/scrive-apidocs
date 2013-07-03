/*
 * Main archive definition. Its a tab based set of different documents lists.
 *
 * Instrument for Mixpanel
 */

(function(window){







var CompanyAccountsModel = Backbone.Model.extend({
  userFullName : function(user) {
    var fullname = user.field("fullname");
    if (user.field("fullname") == undefined || user.field("fullname").length < 2) {
      fullname = user.field("email");
    }
    return fullname;
  },
  addCompanyAccountButton : function() {
    var self = this;
    return Button.init({
        color: "green",
        size: "tiny",
        text: localization.account.companyAccounts.createNewButtonText,
        onClick: function() {
            mixpanel.track('Click new account');
            var body = jQuery("<div class='account-body'>");
            body.append($("<p></p>").text(localization.account.companyAccounts.createNewModalBody));
            var table = jQuery("<table/>");

            var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.fstname + ":"));
            var fstname = jQuery("<input type='text' name='fstname' autocomplete='off' />");
            tr1.append(jQuery("<td/>").append(fstname));
            table.append(tr1);

            var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.sndname + ":"));
            var sndname = jQuery("<input type='text' name='sndname' autocomplete='off' />");
            tr2.append(jQuery("<td/>").append(sndname));
            table.append(tr2);

            var tr5 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.email + ":"));
            var email = jQuery("<input type='text' name='email' autocomplete='off' />");
            tr5.append(jQuery("<td/>").append(email));
            table.append(tr5);

            body.append(table);


            var popup = Confirmation.popup({
              onAccept : function() {

                 var callback = function(t,e,v) {   e.css("border-color", "red"); };
                 fstname.css("border-color", "");
                 sndname.css("border-color", "");
                 email.css("border-color", "");

                  var vresult = [
                                 fstname.validate(new NameValidation({callback: callback, message: "Wrong first name format!"})),
                                 sndname.validate(new NameValidation({callback: callback, message: "Wrong second name format!"})),
                                 email.validate((new NotEmptyValidation({callback: callback, message: "Email cannot be empty!"})).concat(new EmailValidation({callback: callback})))
                                ];

                  if (_.every(vresult, function(a) {return a;})) {

                    new Submit({
                        url: "/account/companyaccounts/add",
                        method: "POST",
                        fstname : fstname.val(),
                        sndname : sndname.val(),
                        email : email.val(),
                        ajax : true,
                        ajaxsuccess : function(resp) {
                          self.userList().recall();
                          if (JSON.parse(resp).added)
                             new FlashMessage({color: "green", content : localization.account.companyAccounts.companyInviteSent});
                          popup.close();
                        },
                        mixpanel : {name : 'Accept',  props : {'Accept' : 'new account'}}
                    }).sendAjax();
                 }
              },
              title : localization.account.companyAccounts.createNewModalTitle,
              acceptButtonText : localization.account.companyAccounts.createNewModalAcceptButton,
              content  : body
            });
            }
      });
  },
  userList : function() {
        var self = this;
        if (this.get("userList") != undefined) return this.get("userList");
        this.set({ "userList" :  new KontraList({
            name : "CompanyAccountsTable",
            headerExtras: self.addCompanyAccountButton().input(),
            loadOnInit : false,
            schema: new Schema({
              url: "/companyaccounts",
              sorting: new Sorting({ fields: ["fullname",
                                              "email",
                                              "role",
                                              "activated",
                                              "deletable"] }),
              paging: new Paging({}),
              textfiltering: new TextFiltering({ text: "", infotext: localization.account.companyAccounts.search }),
              cells : [
                new Cell({name: localization.account.companyAccounts.columnName, width: "260px", field:"fullname"}),
                new Cell({name: localization.account.companyAccounts.columnEmail, width: "260px", field:"email" }),
                new Cell({name: localization.account.companyAccounts.columnRole, width: "260px", field:"role", special: "rendered",
                          rendering: function(value, idx, user) {
                            var label = localization.account.companyAccounts.roleStandard;
                            if (user.field("role")=="RoleAdmin") {
                              label = localization.account.companyAccounts.roleAdmin;
                            } else if (user.field("role")=="RoleStandard") {
                              label =  localization.account.companyAccounts.roleStandard;
                            } else if (user.field("role")=="RolePending") {
                              label =  localization.account.companyAccounts.rolePending;
                            }
                            if (user.field("isctxuser") || user.field("role")=="RolePending") {
                              return  $("<span>").text(label);
                            } else {
                              return $("<a>").text(label)
                                             .click(function() {
                                                  new Submit({
                                                      url: "/account/companyaccounts/changerole",
                                                      method: "POST",
                                                      makeadmin: user.field("role")=="RoleStandard",
                                                      changeid: user.field("id")
                                                    }).sendAjax(function() { self.userList().recall();});
                                                  return false;
                                             });
                            }
                          }}),
                new Cell({width: "16px", field:"activated", special: "rendered",
                          rendering: function(value, idx, user) {
                            if (!user.field("activated")) {
                              var icon = jQuery("<a>");
                              icon.addClass("remind").addClass("icon").css("margin-top","3px");

                              var popupResendConfirmation = function() {
                                var popup = Confirmation.popup({
                                  onAccept: function() {
                                     mixpanel.track('Click resend confirmation');
                                      var submit = new Submit({
                                        url: "/account/companyaccounts/resend",
                                        ajax : true,
                                        ajaxsuccess : function(resp) {
                                          popup.close();
                                          if (JSON.parse(resp).resent)
                                              new FlashMessage({color: "green", content : localization.account.companyAccounts.companyInviteResent});
                                          self.userList().recall();
                                        },
                                        method: "POST",
                                        resendid: user.field("id"),
                                        resendemail: user.field("email"),
                                        mixpanel : {name : 'Accept',
                                                      props : {'Accept' : 'resend confirmation'}}
                                      }).sendAjax();
                                  },
                                  acceptText: localization.account.companyAccounts.resendModalAccept,
                                  rejectText: localization.cancel,
                                  title: localization.account.companyAccounts.resendModalTitle,
                                  content: $("<p/>").text(localization.account.companyAccounts.resendModalBody + self.userFullName(user) + "?")
                                });
                              };
                              icon.click(popupResendConfirmation);

                              return icon;
                            }
                            return jQuery("<span>");
                          }}),
                new Cell({width: "32px", field:"deletable", special: "rendered",
                          rendering: function(value, idx, user) {
                            if (!user.field("isctxuser")) {
                              if (user.field("deletable")) {
                                return $("<a class='icon delete'>").click(function() {
                                    mixpanel.track('Click delete user');
                                    var text = localization.account.companyAccounts.deleteModalBody + self.userFullName(user) + "?";
                                    var content = jQuery("<p/>").text(text);
                                    console.log("text: " + text);
                                    var popup = Confirmation.popup({
                                      onAccept: function() {
                                        mixpanel.track('Click delete user');
                                        var submit = new Submit({
                                          url: "/account/companyaccounts/remove",
                                          method: "POST",
                                          ajax : true,
                                          ajaxsuccess : function(resp) {
                                            popup.close();
                                            if (JSON.parse(resp).removed)
                                              new FlashMessage({color: "green", content : localization.account.companyAccounts.companyAccountDeleted});
                                            else
                                              new FlashMessage({color: "red", content : localization.account.companyAccounts.deleteFailedHasDocuments});
                                            self.userList().recall();
                                          },
                                          removeid: user.field("id"),
                                          removeemail: user.field("email"),
                                          mixpanel : {name : 'Accept', props : {'Accept' : 'delete user'}}
                                        }).sendAjax();

                                      },
                                      acceptText: localization.ok,
                                      rejectText: localization.cancel,
                                      title: localization.account.companyAccounts.deleteModalTitle,
                                      content: content
                                    });
                                    return false;
                                  }
                                );
                              } else {
                                var icon = jQuery("<span class='icon delete gray'>");
                                ToolTip.set({ on: icon,
                                              tip: localization.account.companyAccounts.deleteFailedHasDocuments });
                                return icon;
                              }
                            } else {
                              return $("<span>");
                            }
                        }})
              ]
            })
          })
        });
        return this.userList();
  },
  refresh : function() {
       this.userList().recall();
  }
});



var CompanyAccountsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {
       var container = $(this.el);
       this.el = this.model.userList().el();
       return this;
    }
});


window.CompanyAccounts = function(args) {
          var model = new CompanyAccountsModel(args);
          var view =  new CompanyAccountsView({model : model});
          return {
              refresh : function() {model.refresh();},
              el  : function() {return $(view.el);}
            };
};

})(window);
