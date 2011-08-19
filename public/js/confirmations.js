/* Basic buttons
 * Usage
 *  var button =  Button.init({
 *                   color: "red | green | blue | black",
 *                   size: "tiny | small | big",
 *                   text: "Text that will be put inside of button"
 *                   onClick* : "Function to be called when button is clicked" })
 *  will return Button object. 
 *
 * It exports method input that returns jQuery object to be inserted anywere you want
 *
 * button.input()
*/
  
$(function(){

/* InfoTextInput model. Has value, infotext and information if its focused  */
var ConfirmationModel = Backbone.Model.extend({
  defaults : {
      submit : new Submit(),
      title  : "",
      acceptText: "Ok",
      rejectText: "Cancel",
      acceptColor : "green",
      content  : jQuery("<p/>")
  },
  title : function(){
       return this.get("title");
  },
  content : function(){
       return this.get("content");
  },
  accept : function(){
       return this.get("submit").send();
  },
  acceptText: function() {
       return this.get("acceptText");
  },
  acceptColor: function() {
       return this.get("acceptColor");
  },
  rejectText: function() {
       return this.get("rejectText");
  },
  acceptButton: function() {
      return this.get("acceptButton");
  }
});


var ConfirmationView = Backbone.View.extend({
  events: {
        "click .close"  :  "reject"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject');
        this.model.view = this;
        this.render();
    },
    render: function () {
       var model = this.model;
       this.el.addClass("modal-container");
       var header = $("<div class='modal-header'><span class='modal-icon message'></span></div>");
       var title = $("<span class='modal-title'/>");
       title.append($("<h2/>").append(this.model.title()));
       header.append(title);
       header.append("<a class='modal-close close'/a>");
       var body = $("<div class='modal-body'>");
       var content = $("<div class='modal-content'>");
       content.html(this.model.content());
       body.append(content);
       var footer = $("<div class='modal-footer'>");
       var cancel = $("<a class='cancel close float-left'/>");
       cancel.text(this.model.rejectText());
       footer.append(cancel);
       var accept = model.acceptButton() != undefined ?  model.acceptButton().addClass("float-right") :
            Button.init({color:model.acceptColor(),
                                 size: "small",
                                 cssClass: "float-right",
                                 text: this.model.acceptText(),
                                 onClick : function() { model.accept(); }
            }).input();
       footer.append(accept);
       this.el.append(header);
       this.el.append(body);
       this.el.append(footer);
       return this;
    },
    reject: function(){
        this.clear();
    },
    clear: function(){
        this.model.destroy();
        this.el.remove();
    }

});


window.Confirmation = {
    popup: function (args) {
          var model = new ConfirmationModel({
                      submit : args.submit,
                      title  : args.title,
                      acceptText: args.acceptText,
                      acceptColor : args.acceptColor,
                      rejectText: args.rejectText,
                      content  : args.content,
                      acceptButton : args.acceptButton
                    });
          var overlay = $("<div/>");
          var view = new ConfirmationView({model : model, el : overlay});
          $("body").append(overlay);
          overlay.overlay({load: true,
                           target:overlay,
                           speed : 0,
                           mask: standardDialogMask,
                           top: standardDialogTop,
                           fixed: false      
                          });
   }
    
};

});
