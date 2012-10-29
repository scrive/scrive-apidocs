/* Main archive definition. Its a tab based set of different documents lists. */

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
    return Button.init({
        color: "green",
        size: "tiny",
        text: localization.account.companyAccounts.createNewButtonText,
        onClick: function() {

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


            Confirmation.popup({
              onAccept : function() {
                            new Submit({
                                url: "/account/companyaccounts",
                                method: "POST",
                                add : "true",
                                fstname : fstname.val(),
                                sndname : sndname.val(),
                                email : email.val()
                                }).send();
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
        this.set({ "userList" :  KontraList().init({
            name : "CompanyAccountsTable",
            headerExtras: self.addCompanyAccountButton().input(),
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
                new Cell({name: localization.account.companyAccounts.columnName, width: "100px", field:"fullname"}),
                new Cell({name: localization.account.companyAccounts.columnEmail, width: "100px", field:"email" }),
                new Cell({name: localization.account.companyAccounts.columnRole, width: "100px", field:"role", special: "rendered",
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
                              var role = jQuery("<span>");
                              role.text(label);
                              return role;
                            } else {
                              var role = jQuery("<a>");
                              role.text(label);

                              var submitRoleChange = function() {
                                (new Submit({
                                  url: "$currentlink$",
                                  method: "POST",
                                  changerole: true,
                                  makeadmin: user.field("role")=="RoleStandard",
                                  changeid: user.field("id")
                                })).send();
                              }
                              role.click(submitRoleChange);
                              return role;
                            }
                          }}),
                new Cell({width: "16px", field:"activated", special: "rendered",
                          rendering: function(value, idx, user) {
                            if (!user.field("activated")) {
                              var icon = jQuery("<a>");
                              icon.addClass("reminderForSendIcon");

                              var popupResendConfirmation = function() {
                                var submit = new Submit({
                                  url: "$currentlink$",
                                  method: "POST",
                                  resend: "true",
                                  resendid: user.field("id"),
                                  resendemail: user.field("email")
                                });
                                var text = localization.account.companyAccounts.resendModalBody + self.userFullName(user) + "?";
                                var content = jQuery("<p/>").text(text);
                                console.log("text: " + text);
                                Confirmation.popup({
                                  submit: submit,
                                  acceptText: localization.account.companyAccounts.resendModalAccept,
                                  rejectText: localization.cancel,
                                  title: localization.account.companyAccounts.resendModalTitle,
                                  content: content
                                });
                              };
                              icon.click(popupResendConfirmation);

                              return icon;
                            }
                            return jQuery("<span>");
                          }}),
                new Cell({width: "16px", field:"deletable", special: "rendered",
                          rendering: function(value, idx, user) {
                            if (!user.field("isctxuser")) {
                              if (user.field("deletable")) {
                                var icon = jQuery("<a>");
                                icon.addClass("icon");
                                icon.addClass("delete");

                                var popupDeleteConfirmation = function() {
                                  var submit = new Submit({
                                    url: "$currentlink$",
                                    method: "POST",
                                    remove: "true",
                                    removeid: user.field("id"),
                                    removeemail: user.field("email")
                                  });
                                  var text = localization.account.companyAccounts.deleteModalBody + self.userFullName(user) + "?";
                                  var content = jQuery("<p/>").text(text);
                                  console.log("text: " + text);
                                  Confirmation.popup({
                                    submit: submit,
                                    acceptText: localization.ok,
                                    rejectText: localization.cancel,
                                    title: localization.account.companyAccounts.deleteModalTitle,
                                    content: content
                                  });
                                };
                                icon.click(popupDeleteConfirmation);

                                return icon;
                              } else {
                                var icon = jQuery("<span>");
                                icon.addClass("icon");
                                icon.addClass("delete");
                                icon.addClass("gray");

                                ToolTip.set({ on: icon,
                                              tip: localization.account.companyAccounts.deleteFailedHasDocuments });
                                return icon;
                              }
                            } else {
                              return jQuery("<span>");
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
       container.append(this.model.userList().view.el);
       return this;
    }
});


window.CompanyAccounts = function(args) {
          var model = new CompanyAccountsModel(args);
          var view =  new CompanyAccountsView({model : model, el : $("<div/>")});
          return {
              refresh : function() {model.refresh();},
              el  : function() {return $(view.el);}
            };
};

})(window);
