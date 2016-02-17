var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;
var Submit = require("./submits.js").Submit;

/* Main and only flash messages module
 * Usage
 *   new FlashMessages({ type : "success" | "error",
 *                       content: "Text to be displayed"
 *                       withReload: true/false})
 *
 * If you need to redirect user to different page, and then he should see a flash message, user parameters
 * withRedirect = true and  redirect = new_url
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
    if (args.withReload != undefined && args.withReload == true) {
      new Submit({
        method: "POST",
        url: "/api/frontend/addflash",
        ajax: true,
        type: args.type,
        content: args.content,
        ajaxsuccess: function () {window.location.reload();},
        ajaxerror: function () {window.location.reload();}
      }).send();
      return;
    }

    if (args.withRedirect != undefined && args.withRedirect == true && args.redirect != undefined) {
      new Submit({
        method: "POST",
        url: "/api/frontend/addflash",
        ajax: true,
        type: args.type,
        content: args.content,
        ajaxsuccess: function () {
          // If we only change hash - it will not reload page. hashchange may also trigger some default action (like
          // change content of tab). To avoid this, we block unbind hashchange, change location with hash and force
          // reload.
          if (args.hashChangeOnly) {
            $(window).unbind("hashchange");
            window.location = args.redirect;
            location.reload(true);
          } else {
            window.location = args.redirect;
          }
        },
        ajaxerror: function () {window.location = args.redirect;}
      }).send();
      return;
    }

    var model = new FlashMessageModel(args);
    var $el = $("<div class='flash' />");
    var view = new FlashMessageView({model: model, el: $el});

    setTimeout(function () { view.show(); }, 100);
    setTimeout(function () { view.clear(); }, 10000);
  };

  window.FlashMessagesCleaner = function () {
    $(".flash").css("display", "none");
  };
