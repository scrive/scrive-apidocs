
(function (window) {

window.OAuthRequest = Backbone.Model.extend({
  send: function (f) {
    var request = this;
    $.ajax(Scrive.serverUrl() + request.requestUrl(), {
      headers: {authorization: request.authorization()},
      data: request.data(),
      success: function (res) {
        var arr = res.split("&");
        var resObj = {};
        for (var i = 0; i < arr.length; i++) {
          var bits = arr[i].split("=");
          resObj[bits[0]] = bits[1];
        }
        f(resObj);
        return false;
      },
      type: "GET",
      cache: false,
      responseType: "x-www-form-urlencoded"
    });
  }
});

/* This request is done at the begining, using ajax */
window.OAuthTemporaryCredentialRequest = OAuthRequest.extend({
  defaults: {
    oauth_consumer_key:"",
    oauth_callback: window.location.href,
    oauth_client_shared_secret:"",
  },
  requestUrl: function () {
    return "/oauth/temporarycredentials";
  },
  authorization: function () {
    return "oauth_signature_method=\"PLAINTEXT\"," +
           "oauth_consumer_key=\"" + this.get("oauth_consumer_key") + "\"," +
           "oauth_callback=\""     + this.get("oauth_callback")     + "\"," +
           "oauth_signature=\""    + this.get("oauth_client_shared_secret") + "&aaaaaaaaaaa"    + "\"";
  },
  data: function () {
    return {
      privileges: this.get("priviliges")
    };
  }
});

/* After OAuthTemporaryCredentialRequest is done, user has to be redirected to
 * this adress to grant privilages to service
 */
window.OAuthResourceOwnerAuthorization =  Backbone.Model.extend({
  defaults: {
    oauth_token:"",
  },
  requestUrl: function () {
    return Scrive.serverUrl() + "/oauth/authorization?oauth_token=" + this.get("oauth_token");
  }
});

/* After user confirmed identity with Scrive, this one is used to get key to authorized user requests*/
window.OAuthTokenRequest = OAuthRequest.extend({
  defaults: {
    oauth_consumer_key:"",
    oauth_token: "",
    oauth_verifier: "",
    oauth_client_shared_secret:"",
    oauth_token_secret: "",
  },
  requestUrl: function () {
    return "/oauth/tokencredentials";
  },
  authorization: function () {
    return "oauth_signature_method=\"PLAINTEXT\"," +
           "oauth_consumer_key=\"" + this.get("oauth_consumer_key") + "\"," +
           "oauth_signature=\""    + this.get("oauth_client_shared_secret") + "&" + this.get("oauth_token_secret") + "\"," +
           "oauth_verifier=\""     + this.get("oauth_verifier")     + "\"," +
           "oauth_token=\""        + this.get("oauth_token")    + "\"";
  },
  data: function () {
    return {};
  }
});

})(window);
