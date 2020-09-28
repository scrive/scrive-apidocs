
(function (window) {

window.ApiDemoAuthorizeView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");
    this.model.oauth().bind("change", this.render);
    this.render();
  },
  authorizationSelect: function () {
    var self = this;
    var oauth = self.model.oauth();
    var wrapper = $("<div/>");
    wrapper.append("<label for='authorizationMethodSelect'>Authorisation method</label>");
    var select = $("<select class='form-control' id='authorizationMethodSelect'/>");
    select
      .append($("<option value='email'>Email and password</option>").attr("selected", oauth.emailMode()))
      .append($("<option value='oauth'>OAuth</option>").attr("selected", oauth.oauthMode()))
      .append($("<option value='credentials'>Access credentials</option>").attr("selected", oauth.credentialsMode()));
    select.change(function () {
      oauth.setMode(select.val());
    });
    wrapper.append($("<p/>").append(select));
    return wrapper;
  },
  priviligesSelect: function () {
    var self = this;
    var oauth = self.model.oauth();
    var wrapper = $("<div/>");
    wrapper.append("<label for='priviligesSelect'>Priviliges</label>");
    var select = $("<select class='form-control' id='priviligesSelect'/>");
    select.append(
      $("<option value='DOC_CREATE'>DOC_CREATE</option>")
        .attr("selected", oauth.priviliges() == "DOC_CREATE"));
    select.append(
      $("<option value='DOC_SEND'>DOC_SEND</option>")
        .attr("selected", oauth.priviliges() == "DOC_SEND"));
    select.append(
      $("<option value='DOC_CHECK'>DOC_CHECK</option>")
        .attr("selected", oauth.priviliges() == "DOC_CHECK"));
    select.append(
      $("<option value='DOC_CREATE+DOC_SEND'>DOC_CREATE+DOC_SEND</option>")
        .attr("selected", oauth.priviliges() == "DOC_CREATE+DOC_SEND"));
    select.append(
      $("<option value='DOC_CREATE+DOC_SEND+DOC_CHECK'>DOC_CREATE+DOC_SEND+DOC_CHECK</option>")
        .attr("selected", oauth.priviliges() == "DOC_CREATE+DOC_SEND+DOC_CHECK"));
    select.append(
      $("<option value='FULL_ACCESS'>FULL_ACCESS</option>")
        .attr("selected", oauth.priviliges() == "FULL_ACCESS"));
    select.change(function () {
      oauth.set_priviliges(select.val());
    });
    wrapper.append($("<p/>").append(select));
    return wrapper;
  },
  appendRow: function (rowClass, target, object) {
    target.append($("<div class='row'/>")
          .addClass(rowClass)
             .append($("<div class='col-xs-12'/>")
                  .append(object)
              )
    );
  },
  appendCenteredRow: function (target, object) {
    this.appendRow("text-center", target, object);
  },
  appendLeftRow: function (target, object) {
    this.appendRow("text-left", target, object);
  },
  formInput: function (type, id, label, placeholder, getter, setter, onEnter) {
    var wrapper = $("<div/>");
    wrapper.append("<label for='" + id + "'>" + label + "<label>");
    var input = $("<input class='form-control' />").attr("id", id).attr("type", type).attr("placeholder", placeholder);
    input.val(getter());
    if (setter != undefined) {
      input.change(function () {
        setter(input.val());
      });
    } else {
      input.attr("disabled", true);
    }

    if (onEnter != undefined) {
      input.keypress(function (e) {
        if (e.which == 13) {
          setTimeout(onEnter, 10);
        }
      });
    }

    wrapper.append($("<p/>").append(input));
    return wrapper;
  },
  render: function () {
    var self = this;
    var model = this.model;
    var oauth = model.oauth();
    var container = $("<div class='container'/>");
    self.appendCenteredRow(container, "<img src='img/logo.png'/>");
    self.appendCenteredRow(container, "<div class='divider-line'/>");
    self.appendCenteredRow(container, "<p>E-signing powered by Scrive</p>");
    self.appendLeftRow(container, self.authorizationSelect());

    if (oauth.emailMode()) {
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "email",
          "Email",
          "Email",
          function () { return oauth.email(); },
          function (v) { return oauth.set_email(v); }
        )
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "password",
          "password",
          "Password",
          "Password",
          function () { return oauth.password(); },
          function (v) { return oauth.set_password(v); },
          function () { oauth.tryToGetPesonalToken(); }
        )
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "totp",
          "totp",
          "Two-factor code (only if set for user)",
          "Two-factor code",
          function () { return oauth.totp(); },
          function (v) { return oauth.set_totp(v); },
          function () { oauth.tryToGetPesonalToken(); }
        )
      );
      self.appendCenteredRow(container, $("<p/>").append(
          $("<button class='btn btn-block' type='button'>Authorise</button>")).click(function () {
            oauth.tryToGetPesonalToken();
          })
        );

    } else if (oauth.oauthMode()) {
      self.appendLeftRow(container, $("<p><small>You can generate client credentials at </small></p>")
        .append("<a href='/account#api-dashboard'><small>https://scrive.com/account#api-dashboard</small></a>")
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "oauth_consumer_key",
          "Client credentials identifier",
          "oauth_consumer_key",
          function () { return oauth.consumer_key(); },
          function (v) { return oauth.set_consumer_key(v); }
        )
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "oauth_signature",
          "Client credentials secret",
          "oauth_signature",
          function () { return oauth.client_shared_secret(); },
          function (v) { return oauth.set_client_shared_secret(v); }
        )
      );
      self.appendLeftRow(container, self.priviligesSelect());
      self.appendCenteredRow(container, $("<p/>").append(
          $("<button class='btn btn-block' type='button'>Send Temporary Credential Request</button>"))
            .click(function () {
              oauth.sendeTCR();
            })
        );
      if (oauth.token() != undefined && oauth.token() != "") {
        self.appendLeftRow(
          container,
          self.formInput(
            "text",
            "oauth_token",
            "Temporary credentials identifier",
            "oauth_token",
            function () { return oauth.token(); }
          )
        );
        self.appendLeftRow(
          container,
          self.formInput(
            "text",
            "oauth_token_secret",
            "Temporary credentials secret",
            "oauth_token_secret",
            function () { return oauth.token_secret(); }
          )
        );

        var rao = new OAuthResourceOwnerAuthorization({oauth_token: oauth.token()});
        var button = $("<a class='btn btn-block' title='" + rao.requestUrl() + "' href='" + rao.requestUrl() + "'/>")
                        .text("Go to confirmation page");
        self.appendCenteredRow(container, $("<p/>").append(button));

        if (oauth.verifier() != undefined && oauth.verifier() != "") {
          button.attr("disabled", true);
          self.appendLeftRow(
            container,
            self.formInput(
              "text",
              "oauth_verifier",
              "Verifier",
              "oauth_verifier",
              function () { return oauth.verifier(); }
            )
          );
          self.appendCenteredRow(container, $("<p/>").append(
                $("<button class='btn btn-block' type='button'>Send Token Request</button>")).click(function () {
                  oauth.sendeTR();
                })
              );
        }

      }

    } else if (oauth.credentialsMode()) {
      self.appendLeftRow(
        container,
        $("<p><small>Bypass OAuth handshake by providing access credentials obtained at </small></p>")
          .append("<a href='/account#api-dashboard'><small>https://scrive.com/account#api-dashboard</small></a>")
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "credentials_consumer_key",
          "Client credentials identifier",
          "",
          function () { return oauth.consumer_key(); },
          function (v) { return oauth.set_consumer_key(v); }
        )
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "credentials_signature",
          "Client credentials secret",
          "",
          function () { return oauth.client_shared_secret(); },
          function (v) { return oauth.set_client_shared_secret(v); }
        )
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "credentials_token",
          "Token credentials identifier",
          "",
          function () { return oauth.final_token(); },
          function (v) { return oauth.set_final_token(v); }
        )
      );
      self.appendLeftRow(
        container,
        self.formInput(
          "text",
          "credentials_token_secret",
          "Token credentials secret",
          "",
          function () { return oauth.final_token_secret(); },
          function (v) { return oauth.set_final_token_secret(v); },
          function () { oauth.trigger("change"); }
        )
      );
      self.appendCenteredRow(container, $("<p/>").append(
          $("<button class='btn btn-block' type='button'>Authorise</button>")).click(function () {
            oauth.trigger("change");
          })
        );
    }
    self.appendCenteredRow(container, $("<p/>").append("<BR/>").append($("<a>Skip authorisation</a>")
      .click(function () {
        oauth.clear();
        model.setMode("explore");
        window.location.hash = "#" + model.selectedApiCall().urlHash();

      })));

    $(this.el).addClass("main").empty().append(container);
    return this;
  }
});

})(window);
