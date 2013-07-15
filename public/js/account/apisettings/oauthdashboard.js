
(function(window){


var OauthDashboardModel = Backbone.Model.extend({
  personalTokensTable : function() {
        if (this.get("personalTokensTable") != undefined) return this.get("personalTokensTable");
        this.set({ "personalTokensTable" : this.newPersonalTokensTable() });
        return this.personalTokensTable();
  },
  grantedPrivilegesTable : function() {
        if (this.get("grantedPrivilegesTable") != undefined) return this.get("grantedPrivilegesTable");
        this.set({ "grantedPrivilegesTable" : this.newGrantedPrivilegesTable() });
        return this.grantedPrivilegesTable();
  },
  apiTokensTable : function() {
        if (this.get("apiTokensTable") != undefined) return this.get("apiTokensTable");
        this.set({ "apiTokensTable" : this.newApiTokensTable() });
        return this.apiTokensTable();
  },
  newPersonalTokensTable : function() {
        var self = this;
        return new KontraList({
          name : "Personal Access Token",
          bottomExtras : function(elemCount) {
            if (elemCount == 0)
              return $("<div style='margin-top:5px;margin-bottom:5px;'/>").append(Button.init({
                              size: "tiny",
                              color: "green",
                              text: localization.apiDashboard.personalTokenCreate,
                              onClick: function() {
                                new Submit({
                                  url: "/oauth/createpersonaltoken",
                                  method: "POST",
                                  ajax: true,
                                  ajaxsuccess: function() {self.personalTokensTable().recall()}
                                }).send();
                              }
                            }).input());
          },
          schema: new Schema({
          url: "/oauth/dashboard/personaltoken",
          cells : [
              new Cell({name: "Client credentials identifier",     width:"120px", field:"apitoken"}),
              new Cell({name: "Client credentials secret",    width:"120px",  field:"apisecret", special: "rendered" ,
                      rendering: function(token) {
                          return $("<span style='cursor: pointer; color: #7A94B8'>").text(localization.apiDashboard.reveal).click(function() {
                            $(this).replaceWith($("<span>").text(token));
                          });

                      }
              }),
              new Cell({name: "Token credentials identifier",  width:"120px",  field:"accesstoken"}),
              new Cell({name: "Token credentials secret", width:"120px",  field:"accesssecret", special: "rendered" ,
                      rendering: function(token) {
                          return $("<span style='cursor: pointer; color: #7A94B8'>").text(localization.apiDashboard.reveal).click(function() {
                            $(this).replaceWith($("<span>").text(token));
                          });

                      }
              }),
              new Cell({name: "",              width:"70px",  field:"accesssecret", special: "rendered",
                      rendering: function(status,idx,listobject) {
                        return $("<label class='delete'/>").text(localization.apiDashboard.deleteToken).click(function() {
                             new Submit({
                                url: "/oauth/deletepersonaltoken",
                                method: "POST",
                                ajax: true,
                                ajaxsuccess: function() {self.personalTokensTable().recall()}
                              }).send();
                              return false;
                        });
                      }})
          ]})});
  },
  newGrantedPrivilegesTable : function() {
        var self = this;
        return new KontraList({
          name : "Granted Privileges",
          schema: new Schema({
          url: "/oauth/dashboard/grantedprivileges",
          cells : [
              new Cell({name: localization.apiDashboard.personOrCompany, width:"240px",  field:"name"}),
              new Cell({name: localization.apiDashboard.privilige,          width:"240px", field:"privilegedescription"}),
              new Cell({name: "",                   width:"70px",  field:"privilege", special: "rendered",
                      rendering: function(priv,idx,listobject) {
                              return $("<label class='delete'/>").text(localization.apiDashboard.deleteToken).click(function() {
                                  new Submit({
                                    tokenid: listobject.get("fields").tokenid,
                                    privilege: listobject.get("fields").privilege,
                                    url: "/oauth/deleteprivilege",
                                    method: "POST",
                                    ajax: true,
                                    ajaxsuccess: function() {self.grantedPrivilegesTable().recall()}
                                  }).send();
                                  return false;
                               });

                      }})
          ]})});

  },
  newApiTokensTable : function() {
        var self = this;
        return  new KontraList({
          name : "Granted Privileges",
          bottomExtras: jQuery("<div style='margin-top:5px;margin-bottom:5px;'/>").append(Button.init({
                            size: "tiny",
                            color: "green",
                            text: localization.apiDashboard.apiTokenCreate,
                            onClick: function() {
                              new Submit({
                                url: "/oauth/createapitoken",
                                method: "POST",
                                ajax: true,
                                ajaxsuccess: function() {self.apiTokensTable().recall()}
                              }).send();
                            }
                          }).input()),
          schema: new Schema({
          url: "/oauth/dashboard/apitokens",
          cells : [
              new Cell({name: "Client credentials identifier",  width:"240px",  field:"apitoken"}),
              new Cell({name: "Client credentials secret", width:"240px", field:"apisecret", special: "rendered" ,
                        rendering: function(token) {
                            return $("<span style='cursor: pointer; color: #7A94B8'>").text(localization.apiDashboard.reveal).click(function() {
                              $(this).replaceWith($("<span>").text(token));
                            });

                        }
              }),
              new Cell({name: "",           width:"70px",  field:"apitoken", special: "rendered",
                      rendering: function(apitoken,idx,listobject) {
                          return $("<label class='delete'/>").text(localization.apiDashboard.deleteToken).click(function() {
                             new Submit({
                                apitoken: apitoken,
                                url: "/oauth/deleteapitoken",
                                method: "POST",
                                ajax: true,
                                ajaxsuccess: function() {self.apiTokensTable().recall()}
                              }).send();
                             return false;
                          });
                      }})
          ]})});

  }



});

var OauthDashboardView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {
      var model = this.model;

      var section1 = $("<div class='oauth-section'>")
                      .append($("<label class='oauth-section-header'/>").text(localization.apiDashboard.grandedTokens))
                      .append($("<label class='oauth-section-label'/>").text(localization.apiDashboard.grantedPriviligesDescription))
                      .append($("<div/>").append(model.grantedPrivilegesTable().el()));

      var section2 = $("<div class='oauth-section'>")
                      .append($("<label class='oauth-section-header'/>").text(localization.apiDashboard.personalTokens))
                      .append($("<label class='oauth-section-label'/>").text(localization.apiDashboard.personalTokenDescription))
                      .append($("<div/>").append(model.personalTokensTable().el()));

      var section3 = $("<div class='oauth-section'>")
                      .append($("<label class='oauth-section-header'/>").text(localization.apiDashboard.apiTokens))
                      .append($("<label class='oauth-section-label'/>").text(localization.apiDashboard.apiTokenDescription))
                      .append($("<div/>").append(model.apiTokensTable().el()));

       $(this.el).append(section1).append(section2).append(section3);
    }
});


window.OauthDashboard = function(args) {
          var model = new OauthDashboardModel(args);
          var view =  new OauthDashboardView({model : model, el : $("<div class='tab-container apidashboard'/>")});
          this.el  = function() {return $(view.el);}
          this.refresh  = function() {return;}

};

})(window);

