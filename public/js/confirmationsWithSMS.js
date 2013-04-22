/* We have a lot of popups where body if a preview of the mail.
 * This modal downloads email preview from server and shows such modal allowing edit of some part of email
 *
*/

$(function(){

/* InfoTextInput model. Has value, infotext and information if its focused  */
window.SMS = Backbone.Model.extend({
    defaults: {
        ready : false,
        content : jQuery("<div/>"),
        editable : false,
        editWidth: 540
    },
    initialize: function (args) {
        var sigid = args.signatory != undefined ? args.signatory.signatoryid() : "0";
        /* this.url = "/mailpreview/" + args.document.documentid() + "/" + sigid;
           this.fetch({data : {mailtype: args.type},   processData:  true, cache : false});
        */
        this.set({
            content : $("<div>").text(localization.reminder.mobileQuestion),
            ready : true
        });
    },
    parse: function(response) {
        this.set({
            content : $("<div>").html(response.content),
            ready : true
        });
        this.trigger('ready');
    },
    editable: function() {
        return this.get("editable");
    },
    makeEditable: function(){
        return this.set({ editable : true});
    },
    content: function() {
        return this.get("content");
    },
    ready: function() {
        return this.get("ready");
    },
    editWidth: function() {
        return this.get("editWidth");
    }
});

window.SMSView = Backbone.View.extend({
    model: SMS,
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
/*
    editableMailContent : function() {
        var view = this;
        var content = this.model.content().clone();
        var editablePart = $(".editable", content);
        var textarea = $("<textarea style='height:0px;border:0px;padding:0px;margin:0px'/>").html(editablePart.html());
        textarea.css("width", this.model.editWidth() + "px");
        var wrapper = $("<div style='margin-bottom:12px;'/>").append(textarea);
        editablePart.replaceWith(wrapper);
        setTimeout( function() {
           view.editor = textarea.tinymce({
                      script_url: '/tiny_mce/tiny_mce.js',
                      theme: "advanced",
                      theme_advanced_toolbar_location: "top",
                      theme_advanced_buttons1: "bold,italic,underline,separator,strikethrough,bullist,numlist,separator,undo,redo,separator,cut,copy,paste",
                      theme_advanced_buttons2: "",
                      convert_urls: false,
                      theme_advanced_toolbar_align: "middle",
                      plugins: "noneditable,paste",
                      valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul",
                      oninit : function(ed) {
                          var body = $('body',$('iframe',content).contentDocument);
                           $(ed.getWin()).scroll(
                              function() {
                                body.css('background', '#fffffe');
                                setTimeout(function() {body.css('background', '#ffffff');},1);
                                return true;
                              });
                      },
                      onchange_callback  : function (inst) {
                                view.customtextvalue = inst.getBody().innerHTML;
                      }
          });
        },100);
        return content;
    },
*/
    render : function() {
        var sms = this.model;
        var container = $(this.el);
        if (!sms.ready()) {
            container.addClass('loadingMail');
        }
        else {
            container.removeClass('loadingMail');
            container.empty();
            if (!sms.editable()) {
                container.append(sms.content());
            } else {
                container.append(this.editableMailContent());
            }
            container.find('.notclickableinpreview').click(function() {
                return false;
            });
        }
        return this;
    },
    customtext : function() {
        if (this.editor != undefined)
            return this.editor.val();
        if (this.customtextvalue != undefined)
            return this.customtextvalue;
        return "";
    }
});

var ConfirmationWithSMSModel = Backbone.Model.extend({
  defaults : {
      onAccept : function() {},
      title  : "",
      acceptText: "Ok",
      rejectText: "Cancel",
      editText : "Edit",
      acceptColor : "green",
      onEdit: function() {},
      textfont : undefined,
      textcolor : undefined
  },
  initialize : function() {
  },
  title : function(){
       return this.get("title");
  },
  accept  : function(customtext){
       return this.get("onAccept")(customtext);
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
  textfont: function() {
      return this.get("textfont");
  },
  textcolor: function() {
      return this.get("textcolor");
  },
  editText: function() {
       return this.get("editText");
  },
  sms: function() {
       return this.get("sms");
  },
  onEdit: function() {
      return this.get("onEdit");
  }

});


var ConfirmationWithSMSView = Backbone.View.extend({
    events: {
        "click .close" :  "reject",
        "click .edit"  :  "edit"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject');
        this.model.view = this;
        this.render();
    },
    render: function () {
       var model = this.model;
       var view = this;
       var container = $("<div class='modal-container' style='width:800px'/>");
       container.css("top",$(window).scrollTop());
       container.css("margin-top",50);
       container.css("left",$(window).scrollLeft());
       container.css("margin-left",Math.floor(($(window).width() - 800) / 2));

       //Modal header
       var header = $("<div class='modal-header'><span class='modal-icon message'></span></div>");
       var title = $("<span class='modal-title'/>");
       if (model.textcolor())
         title.css("color",model.textcolor());
       if (model.textfont())
         title.css("font-family",model.textfont());
       title.append(this.model.title());
       header.append(title);
       header.append($("<a class='modal-close'></a>").click(function() {view.reject(); return false;}));


       //Modal body
       var body = $("<div class='modal-body'>");
       var content = $("<div class='modal-content'>");
       var smsview = new SMSView({model: model.sms(), el : $("<div/>")});
       content.append($("<div class='body'/>").html($(smsview.el)));
       body.append(content);

       //Modal footer
       var footer = $("<div class='modal-footer'>");
       var cancelOption = $("<label class='clickable cancel close float-left' s/>");
       cancelOption.text(this.model.rejectText());
       this.editOption = $("<label class='clickable edit float-left' style='margin-left:10px;'/>");
       this.editOption.text(this.model.editText());
       if (model.textfont()) {
         cancelOption.css("font-family",model.textfont());
         this.editOption.css("font-family",model.textfont());
       }
       footer.append(cancelOption);
       /*
        * We will allow changes to SMS texts, but that is feature for later.
        */
       // footer.append(this.editOption);
       var accept = Button.init({color:model.acceptColor(),
                                 size: "tiny",
                                 cssClass: "float-right",
                                 shape: "rounded",
                                 text: this.model.acceptText(),
                                 onClick : function() {
                                     var customtext = smsview.customtext();
                                     var res = model.accept(customtext);
                                     if (res == true) view.reject(); //We don't actually reject. Just clean the modal.

                                }
        });
       footer.append(accept.input());
       container.append(header);
       container.append(body);
       container.append(footer);
       $(this.el).append(container);
       return this;
    },
    reject: function(){
        var self = this;
        $(this.el).removeClass("active");
        setTimeout(function() {if (self.clear != undefined) self.clear()}, 600);
    },
    edit: function(){
        this.editOption.remove();
        this.model.sms().makeEditable();
                this.model.onEdit()();
        return false;
    },
    clear: function(){
        if (this.model != undefined) {
          this.model.destroy();
          this.model.view = undefined;
        }
        if (this.el != undefined)
          $(this.el).remove();
    }

});


window.ConfirmationWithSMS = {
    popup: function (args) {
          var model = new ConfirmationWithSMSModel(args);
          var overlay = $("<div class='modal'/>");
          overlay.height($(document).height());
          var view = new ConfirmationWithSMSView({model : model, el : overlay});
          $("body").append(overlay);
          setTimeout(function() {overlay.addClass("active");},100);
          return model;
   }

};

});
