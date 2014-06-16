/* Main and only flash messages module
 * Usage
 *   new FlashMessages({ color : "red | green | blue",
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
       if (!(attr.color == "red" || attr.color == "blue" || attr.color == "green"))
            console.log("FlashMessage error: Bad color selected ( "  + attr.color +" )");
  },
  flashType : function() {
    if (this.get("color") == "red")
      return "error";
    else if (this.get("color") == "blue")
      return "success";
    else
      return "success";
  }
});

var FlashMessageView = Backbone.View.extend({

    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.render();
    },
    render: function () {
        var self = this;
        $(this.el).addClass(this.model.flashType());
        var close = $("<img width='24' height='24' src='/img/X-white.png'/>");
        close.click(function() {
          self.clear();
          return false;
        });
        $(this.el).append($("<div class='flash-content-wrapper'> </div>").append($("<div class='flash-content'> </div>")
                    .append("<div class='flash-icon-wrapper'><div class='flash-icon'></div></div>")
                    .append($("<div class='flash-body'></div>").append(this.model.get("content")))
                    .append($("<div class='flash-close'></div>").append(close)))
                   );
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
    }
});

window.FlashMessage = function(args) {
        if (args.withReload != undefined && args.withReload == true) {
          new Submit({
            method : "POST",
            url : "/api/frontend/addflash",
            ajax : true,
            color : args.color,
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
            color : args.color,
            content : args.content,
            ajaxsuccess : function() {window.location = args.redirect;},
            ajaxerror   : function() {window.location = args.redirect;}
          }).send();
          return;
        }
        var model = new FlashMessageModel(args);
        var view = new FlashMessageView({model : model, el : $("<div class='flash'/>")});
        $("body").append($(view.el));
        if ($('.flash-body',$(view.el)).height() > $('.flash-content',$(view.el)).height() + 10)
          $('.flash-body',$(view.el)).addClass("twoLines");
        setTimeout(function() {$(view.el).addClass("active");},100);
        setTimeout(function() {view.clear();},10000);
};

window.FlashMessagesCleaner = function() {
        $('.flash').css("display","none");
};

});
