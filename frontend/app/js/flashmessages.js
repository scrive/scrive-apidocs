var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;
var Submit = require("./submits.js").Submit;
var Track = require("../scripts/common/track");

/* Main and only flash messages module
 * Usage
 *   new FlashMessage({ type : "success" | "error"
 *                    , content: "Text to be displayed"
 *                    })
 *
 * If you need to redirect user to different page, and then he should see a flash message,
 * use "new FlashMessageAfterReload" (with same parameter) and then reload the page. The flash message
 * will be saved in a cookie and displayed after page is reloaded. You still have to reload
 * the page yourself.
 *
 * If you need to clean all flash message do
 *   new FlashMessagesCleaner();
 */

var FlashMessageModel = Backbone.Model.extend({
  initialize: function (attr) {
    if (!(attr.type == "success" || attr.type == "error")) {
      console.log("FlashMessage error: Bad type selected ( "  + attr.type + " )");
    }
  },

  content: function () {
    return this.get("content");
  },

  type: function () {
    if (this.get("type") == "success") {
      return "success";
    } else if (this.get("type") == "error") {
      return "error";
    } else {
      return "success";
    }
  }
});

var FlashMessageView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render", "clear", "show", "hide");

    $("body").append(this.$el);

    this.render();
    this.hide();
  },

  show: function () {
    this.$el.css("top", 0);
  },

  hide: function () {
    this.$el.css("top", -1 * this.$el.height());
  },

  clear: function () {
    var self = this;

    if (self.$el) {
      self.hide();
    }

    setTimeout(function () {
      if (self.$el) { self.$el.remove(); }
      if (self.model) { self.model.destroy(); }
    }, 1000);
  },

  render: function () {
    var self = this;
    var model = self.model;

    self.$el.empty();
    self.$el.addClass(model.type());

    var close = $("<div class='flash-close'>&times;</div>");

    close.click(function (e) {
      e.preventDefault();
      self.clear();
    });

    self.$el.append($("<div class='flash-content-wrapper' />").append($("<div class='flash-content' />")
      .append($("<div class='flash-body' />").append(model.content())).append(close)));
  }
});

var FlashMessage = exports.FlashMessage = function (args) {
  var model = new FlashMessageModel(args);
  var $el = $("<div class='flash' />");
  var view = new FlashMessageView({model: model, el: $el});

  setTimeout(function () { view.show(); }, 100);
  setTimeout(function () { view.clear(); }, 10000);
};

exports.FlashMessageAfterReload = function (args) {
  Cookies.set("flashmessage", JSON.stringify({"type": args.type, "content": args.content}));
};

exports.FlashMessagesCleaner = function () {
  $(".flash").css("display", "none");
};

var tryUnescapeQuotes = function (s) {
  if (s.charAt(0) === "\"" && s.charAt(s.length - 1) === "\"") {
    return s.substr(1, s.length - 2).replace(/\\"/g, '"');
  } else {
    return s;
  }
};

exports.FlashMessageTryFromCookie = function () {
  if (Cookies.get("flashmessage")) {
    try {
      var jsonFlash = JSON.parse(tryUnescapeQuotes(Cookies.get("flashmessage")));
      jsonFlash["content"] = decodeURIComponent(jsonFlash["content"]);
      new FlashMessage(jsonFlash);
    } catch(err) {
      Track.track("Failed to parse cookie: " + Cookies.get("flashmessage"));
    } finally {
      Cookies.delete("flashmessage");
    }
  }
};
