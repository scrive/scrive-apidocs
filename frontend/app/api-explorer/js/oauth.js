
(function (window) {

window.OAuth = Backbone.Model.extend({
  defaults: {
    callback:             window.location.href, // not saved by save()
    mode:                 "email", // Available modes "email", "oauth", "credentials", ("explore"), ((""))
    password:             "", // not saved by save(), nor by set_password()
    email:                LocalStorage.get("oauth", "email")                 || "",
    consumer_key:         LocalStorage.get("oauth", "consumer_key")          || "",
    client_shared_secret: LocalStorage.get("oauth", "client_shared_secret")  || "",
    token:                LocalStorage.get("oauth", "token")                 || "",
    token_secret:         LocalStorage.get("oauth", "token_secret")          || "",
    verifier:             LocalStorage.get("oauth", "verifier")              || "",
    final_token:          LocalStorage.get("oauth", "final_token")           || "",
    final_token_secret:   LocalStorage.get("oauth", "final_token_secret")    || "",
    priviliges:           LocalStorage.get("oauth", "priviliges")            || "", // had "DOC_CREATE" as default
  },
  save: function () {
    LocalStorage.set("oauth", "mode",                 this.mode());
    LocalStorage.set("oauth", "email",                this.email());
    LocalStorage.set("oauth", "consumer_key",         this.consumer_key());
    LocalStorage.set("oauth", "client_shared_secret", this.client_shared_secret());
    LocalStorage.set("oauth", "token",                this.token());
    LocalStorage.set("oauth", "token_secret",         this.token_secret());
    LocalStorage.set("oauth", "verifier",             this.verifier());
    LocalStorage.set("oauth", "final_token",          this.final_token());
    LocalStorage.set("oauth", "final_token_secret",   this.final_token_secret());
    LocalStorage.set("oauth", "priviliges",           this.priviliges());
  },
  clear: function () {
    this.set({
      //"mode":                 "",  // don't clear
      "email":                "",
      "password":             "",
      //"consumer_key":         "",  // don't clear, may be reused to get new final_token and final_token_secret(?)
      //"client_shared_secret": "",  // don't clear, may be reused to get new final_token and final_token_secret(?)
      "token":                "",
      "token_secret":         "",
      "verifier":             "",
      "final_token":          "",
      "final_token_secret":   "",
      "priviliges":           "" // had "DOC_CREATE" as default
    }, {silent: true});
    this.save();
    this.trigger("change");
  },

  emailMode: function () {
    return this.get("mode") == "email";
  },
  oauthMode: function () {
    return this.get("mode") == "oauth";
  },
  credentialsMode: function () {
    return this.get("mode") == "credentials";
  },
  mode: function () {
    return this.get("mode");
  },
  setMode: function (mode) { // set_mode...
    window.location.hash = mode;
    this.set({"mode": mode});
    this.save();
  },

  /* Email and password for shortcut credentials creation */
  email: function () {
    return this.get("email");
  },
  set_email: function (v) {
    this.set({"email": v}, {silent: true});
    this.save();
  },
  password: function () {
    return this.get("password");
  },
  set_password: function (v) {
    this.set({"password": v}, {silent: true});
    // not saved!! don't save!!
  },

  /* TCR PARAMS*/
  consumer_key: function () {
    return this.get("consumer_key");
  },
  set_consumer_key: function (v) {
    this.set({"consumer_key": v}, {silent: true});
    this.save();
  },
  client_shared_secret: function () {
    return this.get("client_shared_secret");
  },
  set_client_shared_secret: function (v) {
    this.set({"client_shared_secret": v}, {silent: true});
    this.save();
  },
  callback: function () {
    return this.get("callback");
  },
  set_callback: function (v) { // not used...
    this.set({"callback": v}, {silent: true});
    this.save();
  },

  priviliges: function () {
    return this.get("priviliges");
  },
  set_priviliges: function (v) {
    this.set({"priviliges": v}, {silent: true});
    this.save();
  },

  /* ROA PARAMS*/
  token: function () {
    return this.get("token");
  },
  set_token: function (v) {
    this.set({"token": v}, {silent: true});
    this.save();
  },
  token_secret: function () {
    return this.get("token_secret");
  },
  set_token_secret: function (v) {
    this.set({"token_secret": v}, {silent: true});
    this.save();
  },

  /* TR PARAMS*/
  verifier: function () {
    return this.get("verifier");
  },
  set_verifier: function (v) {
    this.set({"verifier": v}, {silent: true});
    this.save();
  },

  /* Final tokens*/
  final_token: function () {
    return this.get("final_token");
  },
  set_final_token: function (v) {
    this.set({"final_token": v}, {silent: true});
    this.save();
  },
  final_token_secret: function () {
    return this.get("final_token_secret");
  },
  set_final_token_secret: function (v) {
    this.set({"final_token_secret": v}, {silent: true});
    this.save();
  },

  sendeTCR: function () {
    var model = this;
    new OAuthTemporaryCredentialRequest({
      oauth_consumer_key: this.consumer_key(),
      oauth_callback:  this.callback(),
      oauth_client_shared_secret: this.client_shared_secret(),
      priviliges: this.priviliges()
    }).send(function (res) {
      model.set_token(res.oauth_token);
      model.set_token_secret(res.oauth_token_secret);
      model.set_verifier(null);
      model.trigger("change");
    });
  },
  sendeTR: function () {
    var model = this;
    new OAuthTokenRequest({
      oauth_consumer_key: this.consumer_key(),
      oauth_client_shared_secret: this.client_shared_secret(),
      oauth_token: this.token(),
      oauth_token_secret: this.token_secret(),
      oauth_verifier: this.verifier()
    }).send(function (res) {
      model.set_final_token(res.oauth_token);
      model.set_final_token_secret(res.oauth_token_secret);
      model.trigger("change");
      model.tryToGetAccessData(null);
    });
  },
  authorizationForRequests: function () {
    return "oauth_signature_method=\"PLAINTEXT\"," +
           "oauth_consumer_key=\"" + this.consumer_key() + "\"," +
           "oauth_token=\""        + this.final_token()     + "\"," +
           "oauth_signature=\""    + this.client_shared_secret() + "&" + this.final_token_secret()    + "\""
  },
  tryToGetPersonalToken: function () {
    var model = this;
    // this call does NOT need Oauth authorisation
    $.ajaxSetup({timeout: 2000}); // $.post does not allow a timeout parameter (and don't want to use the $.ajax call directly (yet))
    $.post(Scrive.serverUrl() + "/api/v1/getpersonaltoken", {email: model.email(), password: model.password()},
          function (rs, status, xhr) {
            $.ajaxSetup({timeout: null}); // $.post does not allow a timeout parameter (and don't want to use the $.ajax call directly (yet))
            if (status == "success") {
              var resp = JSON.parse(rs);
              model.set_consumer_key(resp.apitoken);
              model.set_client_shared_secret(resp.apisecret);
              model.set_final_token(resp.accesstoken);
              model.set_final_token_secret(resp.accesssecret);
              model.trigger("change");
              model.tryToGetAccessData("DOC_CREATE");
            } else {
              model.clear();
            }
          });
  },
  tryToGetAccessData: function (defaultAccessLevel) {
    var model = this;
    //var sess_cook = $.cookie('sessionId');
    //$.cookie("sessionId", null); // be sure not to be "logged in" when doing the getprofile, otherwise you'll get the profile for the login, not the profile for the credentials
    // this call DOES need Oauth authorisation
    $.ajaxSetup({timeout: 2000, headers: { authorization: this.authorizationForRequests() }}); // default... (elsewhere we don't use the default; consider using $.ajax directly and set the authorization header only for this ajax call)
    if (defaultAccessLevel != null) { // is there a (nonpublic?) query API call for getting the access level
      model.set_priviliges(defaultAccessLevel);
    }
    $.get(Scrive.serverUrl() + "/api/v1/getprofile", 
          function (rs, status, xhr) {
            $.ajaxSetup({timeout: null}); // $.get does not allow a timeout parameter (and don't want to use the $.ajax call directly (yet))
            if (status == "success") {
              var resp = JSON.parse(rs);
              model.set_email(resp.email);
              model.trigger("change");
            } else {
              model.clear();
            }
          });
    //$.cookie("sessionId", sess_cook); // reset the login cookie
  },
  
  isSetUp: function () {
    return    this.final_token()          != undefined && this.final_token()          != ""
           && this.final_token_secret()   != undefined && this.final_token_secret()   != ""
           && this.consumer_key()         != undefined && this.consumer_key()         != ""
           && this.client_shared_secret() != undefined && this.client_shared_secret() != "";
  }
});

$.extend({
  getUrlVars: function () {
    var vars = [];
    var hash;
    var hashes = window.location.href.slice(window.location.href.indexOf("?") + 1, window.location.href.indexOf("#"));
    hashes = hashes.split("&");
    for (var i = 0; i < hashes.length; i++)
    {
      hash = hashes[i].split("=");
      vars.push(hash[0]);
      vars[hash[0]] = hash[1];
    }
    return vars;
  },
  getUrlVar: function (name) {
    return $.getUrlVars()[name];
  }
});

})(window);
