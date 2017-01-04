/*
 * API demo main model + view
 */

(function (window) {

var ApiDemoModel = Backbone.Model.extend({
  defaults: {
    mode: "explore",
    willBeReloaded: false
  },
  willBeReloaded: function () {
    return this.get("willBeReloaded");
  },
  authorizeMode: function () {
    return this.get("mode") == "authorize";
  },
  exploreMode: function () {
    return this.get("mode") == "explore";
  },
  setMode: function (v) {
    this.set({"mode": v});
  },
  changeAPIVersion: function (v) {
    if (this.selectedApiCall().callPrototype().apiVersion() != v) {
      var equivalent = this.selectedApiCall().callPrototype().equivalentInVersion(v);
      var apiCalls;
      if (v == "v1") {
        apiCalls = APICalls.apiV1Calls();
      } else {
        apiCalls = APICalls.apiV2Calls();
      }
      var findEquivalent = _.find(apiCalls, function (c) { return c.name() == equivalent; });
      if (findEquivalent != undefined) {
        this.setSelectedApiCall(findEquivalent);
      } else {
        if (v == "v1") {
          this.setSelectedApiCall(APICalls.apiV1Calls()[0]);
        } else {
          var defaultV2Call = _.find(APICalls.apiV2Calls(), function (c) { return c.name() == "Get"; });
          this.setSelectedApiCall(defaultV2Call);
        }
      }
    }
  },
  apiVersion: function () {
    return this.selectedApiCall().callPrototype().apiVersion();
  },
  initialize: function (args) {
    var self = this;
    var oauth = new OAuth();
    if ($.getUrlVar("oauth_verifier") != undefined) {
      oauth.set_verifier($.getUrlVar("oauth_verifier"));
      window.location.href = window.location.href.substring(0, window.location.href.indexOf("?")) + "#oauth";
      self.set("willBeReloaded");
      return;
    }
    var v2NewCallIdx = APICalls.apiV2Calls().findIndex(function (c) { return c.name() == "New"; });
    this.set({oauth: oauth, selectedApiCall: APICalls.apiV2Calls()[v2NewCallIdx].createCall({oauth: oauth})});
    this.listenTo(oauth, "change", function () {
      if (oauth.isSetUp() && self.authorizeMode()) {
        this.setMode("explore");
        this.selectedApiCall().unsend();
        window.location.hash = "#" + this.selectedApiCall().urlHash();

      } else {
        self.trigger("change");
      }
    });
    self.route();
    $(window).bind("hashchange", function (c) {
      self.route();
    });
  },
  route: function () {
    var self = this;
    var oauth = this.oauth();
    if (window.location.hash == "#email") {
      self.setMode("authorize");
      oauth.setMode("email");
    } else if (window.location.hash == "#oauth") {
      self.setMode("authorize");
      oauth.setMode("oauth");

    } else if (window.location.hash == "#credentials") {
      self.setMode("authorize");
      oauth.setMode("credentials");
    } else {
      this.setMode("explore");
      _.each(APICalls.apiV1Calls(), function (c) {
        if (window.location.hash == "#" + c.urlHash()) {
          self.setSelectedApiCall(c);
        }
      });
      _.each(APICalls.apiV2Calls(), function (c) {
        if (window.location.hash == "#" + c.urlHash()) {
          self.setSelectedApiCall(c);
        }
      });
    }
  },
  oauth: function () {
    return this.get("oauth");
  },
  selectedApiCall: function () {
    return this.get("selectedApiCall");
  },
  setSelectedApiCall: function (callPrototype) {
    if (window.location.hash != "#" + callPrototype.urlHash()) {
      window.location.hash = "#" + callPrototype.urlHash();
    }
    this.set({selectedApiCall: callPrototype.createCall({oauth: this.oauth()})});

  }
});

var ApiDemoView = Backbone.View.extend({
  model: ApiDemoModel,
  initialize: function (args) {
    _.bindAll(this, "render");
    this.model.bind("change", this.render);
    this.render();
  },
  cleanup: function () {
    if (this.explorerView) {
      this.explorerView.cleanup();
    }
    $(".credentials-view-placeholder").remove();
    $(".credentials-view").remove();

  },
  render: function () {
    this.cleanup();
    var model = this.model;
    $(this.el).children().detach();
    if (model.willBeReloaded()) {
      return; // If view will be reloaded we don't render anything;
    } else if (model.authorizeMode()) {
      $("body").addClass("authorizeview").removeClass("exploreview");
      $(this.el).append(new ApiDemoAuthorizeView({model: model}).el);
    } else {
      $("body").addClass("exploreview").removeClass("authorizeview");
      this.explorerView = new ApiDemoExploreView({model: model});
      $(this.el).append(this.explorerView.el);

      if (model.oauth().isSetUp()) {
        $("body").append("<div class='credentials-view-placeholder'/>");
        $("body").append(new CredentialsView({
          model: model,
          el: $("<div class='credentials-view container text-center'/>")
        }).el);
      }
    }

    return this;
  }
});

window.ApiDemo = function () {
  var model = new ApiDemoModel({});
  var view  = new ApiDemoView({
    model: model,
    el: $("<div class='main'/>")
  });
  return {
    model: function () { return model; },
    view: function () { return view; }
  };
};

})(window);
