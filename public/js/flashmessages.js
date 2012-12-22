/* Main and only flash messages module
 * Usage
 *   new FlashMessages({ color : "red | green | blue" , content: "Text to be displayed"})
 *
 */

$(function(){

/* Flash mesage contains text and color.
  */
var FlashMessageModel = Backbone.Model.extend({
  initialize: function(attr) {
       if (!(attr.color == "red" || attr.color == "blue" || attr.color == "green"))
            console.log("FlashMessage error: Bad color selected ( "  + attr.color +" )");
        if (!_.isString(attr.content) || attr.content.length === 0 )
            console.log("FlashMessage error: No content provided");
  },
  flashType : function() {
    if (this.get("color") == "red")
      return "error";
    else if (this.get("color") == "blue")
      return "warning";
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
        var close = $("<img width='24' height='24' src='/img/close.png'/>");
        close.click(function() {
          self.clear();
          return false;
        });
        $(this.el).append($("<div class='flash-content'> </div>")
                    .append("<div class='flash-icon'><img width='24' height='24' alt='Icon' src='/img/ux.png'></div>")
                    .append($("<div class='flash-body'></div>").append(this.model.get("content")))
                    .append($("<div class='flash-close'></div>").append(close))
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
        var model = new FlashMessageModel(args);
        var view = new FlashMessageView({model : model, el : $("<div class='flash'/>")});
        $("body").append($(view.el));
        setTimeout(function() {$(view.el).addClass("active");},100);
        setTimeout(function() {view.clear();},10000);
}
});
