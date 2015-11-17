/* Main and only flash messages module
 * Usage
 *   new FlashMessages({ type : "success" | "error",
 *                       content: "Text to be displayed"
 *                       withReload: true/false})
 *
 * If you need to redirect user to different page, and then he should see a flash message, user parameters withRedirect = true and  redirect = new_url
 *
 * If you need to clean all flash message do
 *   new FlashMessagesCleaner();
 */

define(['Backbone', 'legacy_code'], function() {

/* Flash mesage contains text and color.
  */
var FlashMessageModel = Backbone.Model.extend({
  initialize: function(attr) {
       if (!(attr.type == "success" || attr.type == "error"))
            console.log("FlashMessage error: Bad type selected ( "  + attr.type +" )");
  },
  flashType : function() {
    if (this.get("type") == "success")
      return "success";
    else if (this.get("type") == "error")
      return "error";
    else
      return "success";
  }
});

var FlashMessageView = Backbone.View.extend({

    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        $("body").append(this.$el);
        if (BrowserInfo.isIE8orLower()) {
         window.attachEvent("resize", this.render);
        } else {
         window.addEventListener("resize", this.render);
        }
        this.render();
    },
    render: function () {
        var self = this;
        this.$el.empty();
        $(this.el).addClass(this.model.flashType());
        var close = $("<div class='flash-close'>&times;</div>");
        close.click(function() {
          self.clear();
          return false;
        });
        $(this.el).append($("<div class='flash-content-wrapper'> </div>").append($("<div class='flash-content'> </div>")
                    .append($("<div class='flash-body'></div>").append(this.model.get("content")))
                    .append(close))
                   );
        if ($('.flash-body',$(self.el)).height() > $('.flash-content',$(self.el)).height() + 10) {
          console.log("multi line");
          $('.flash-content',$(self.el)).addClass("multiline");
        }
        return this;
    },
    clear: function(){
        var self = this;
        var model = this.model;
        if (this.el != undefined)
          $(this.el).removeClass("active");
        setTimeout(function() {
          if (self.el != undefined) $(self.el).remove();
            if (self.model != undefined) self.model.destroy();
        },1000);
        window.removeEventListener("resize", this.render);
    }
});

window.FlashMessage = function(args) {
        if (args.withReload != undefined && args.withReload == true) {
          new Submit({
            method : "POST",
            url : "/api/frontend/addflash",
            ajax : true,
            type : args.type,
            content : args.content,
            ajaxsuccess : function() {window.location.reload();},
            ajaxerror   : function() {window.location.reload();}
          }).send();
          return;
        }
        if (args.withRedirect != undefined && args.withRedirect == true && args.redirect != undefined) {
          new Submit({
            method : "POST",
            url : "/api/frontend/addflash",
            ajax : true,
            type : args.type,
            content : args.content,
            ajaxsuccess : function() {
              // If we only change hash - it will not reload page. hashchange may also trigger some default action (like change content of tab).
              // To avoid this, we block unbind hashchange, change location with hash and force reload.
              if (args.hashChangeOnly) {
                $(window).unbind('hashchange');
                window.location = args.redirect;
                location.reload(true);
              } else {
                window.location = args.redirect;
              }
            },
            ajaxerror   : function() {window.location = args.redirect;}
          }).send();
          return;
        }
        var model = new FlashMessageModel(args);
        var $el = $("<div />");
        $el.addClass(args.className || "flash");
        var view = new FlashMessageView({model : model, el : $el});

        setTimeout(function() {$(view.el).addClass("active");},100);
        setTimeout(function() {view.clear();},10000);
};

window.FlashMessagesCleaner = function() {
        $('.flash').css("display","none");
};

});
