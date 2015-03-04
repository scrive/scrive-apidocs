/* We have a lot of popups where body if a preview of the mail.
 * This modal downloads email preview from server and shows such modal allowing edit of some part of email
 *
*/

define(['Backbone','legacy_code'], function(Backbone) {

window.Mail = Backbone.Model.extend({
	defaults : {
		ready : false,
		content : jQuery("<div/>"),
		editable : false,
		editWidth: 540
	},
	initialize: function (args) {
		var sigid = args.signatory != undefined ? args.signatory.signatoryid() : "0";
		this.url = "/mailpreview/" + args.document.documentid() + "/" + sigid;
        this.fetch({data : {mailtype: args.type},   processData:  true, cache : false});
    },
	parse : function(response) {
		this.set({
			content : $("<div>").html(response.content),
			ready : true
		});
		this.trigger('ready');
	},
	editable: function() {
		return this.get("editable");
	},
	makeEditable : function(){
		return this.set({ editable : true});
	},
	content : function() {
		return this.get("content");
	},
	ready : function() {
		return this.get("ready");
	},
    editWidth: function() {
                return this.get("editWidth");
    }
});

window.MailView = Backbone.View.extend({
    model : Mail,
    initialize: function (args) {
        _.bindAll(this, 'render');
		this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    editableMailContent : function() {
        var self = this;
        var content = this.model.content().clone();
        var editablePart = $(".editable", content);
        self.textarea = $("<textarea/>");
        self.textarea.css("width", this.model.editWidth() + "px");
        var wrapper = $("<div style='margin-bottom:12px;'/>").append(self.textarea);
        editablePart.replaceWith(wrapper);
        return content;
	},
    render : function() {
	var mail = this.model;
	var container = $(this.el);
	if (!mail.ready()) {
            container.addClass('loadingMail');
        }
        else {
            container.removeClass('loadingMail');
            container.empty();
            if (!mail.editable()) {
                  container.append(mail.content());
            } else {
                  container.append(this.editableMailContent());
                  this.textarea.focus();
             }
            container.find('.notclickableinpreview').click(function() {
              return false;
            });
        }
        return this;
    },
    customtext : function() {
        if (this.textarea != undefined)
            return this.textarea.val();
        return "";
    }
});

var ConfirmationWithEmailModel = Backbone.Model.extend({
  defaults : {
      onAccept : function() {},
      onReject: function() {},
      bottom: false,
      title  : "",
      acceptText: localization.ok,
      rejectText: localization.cancel,
      editText : "Edit",
      acceptType : "action",
      onEdit: function() {}
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
  acceptType: function() {
       return this.get("acceptType");
  },
  rejectText: function() {
       return this.get("rejectText");
  },
  editText: function() {
       return this.get("editText");
  },
  mail: function() {
       return this.get("mail");
  },
  bottom: function() {
       return this.get("bottom");
  },
  onEdit: function() {
      return this.get("onEdit");
  },
  onReject: function() {
      return this.get("onReject");
  },
  close: function() {
      this.view.reject();
  }
});


var ConfirmationWithEmailView = Backbone.View.extend({
  events: {
        "click .close" :  "reject"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject');
        this.model.view = this;
        this.render();
    },
    render: function () {
       var model = this.model;
       var view = this;
       var container = $("<div class='modal-container'/>").css('width',BrowserInfo.isSmallScreen() ? "980px" : "800px");
       this.container = container;
       if(BrowserInfo.isSmallScreen()) container.addClass("small-screen");
       container.css("top",$(window).scrollTop());
       container.css("margin-top",50);
       container.css("left","0px");
       var left = Math.floor(($(window).width() - (BrowserInfo.isSmallScreen() ? 980 : 800)) / 2);
       container.css("margin-left",left > 20 ? left : 20);

	   //Modal header
       var header = $("<div class='modal-header no-icon'></div>");
       var title = $("<span class='modal-title'/>");
       title.append(this.model.title());
       header.append(title);
       header.append($("<a class='modal-close'></a>").click(function() {view.reject(); return false;}));


	   //Modal body
       var body = $("<div class='modal-body'>");
       var content = $("<div class='modal-content'>");
	   var mailview = new MailView({model: model.mail(), el : $("<div/>")});
       content.append($("<div class='body'/>").html($(mailview.el)));
        // added by Eric to not let anything be clickable in the email
        content.find('*').click(function(){
            return false;
        });
       body.append(content);

	   //Modal footer
       var footer = $("<div class='modal-footer'>");
       var cancelOption = $("<label class='clickable cancel close float-left' s/>");
       cancelOption.text(this.model.rejectText());
       this.editOption = new Button({
                                    style: 'margin-left: 15px',
                                    cssClass: 'float-left',
                                    text: this.model.editText(),
                                    onClick: function() {
                                        view.edit();
                                    }
       });

       footer.append(cancelOption);

       if (!BrowserInfo.isSmallScreen()) { // We skip editing message on small screens
         this.editOption = this.editOption.el(); // make it hideable from other places.
         if (model.editText()) {
            footer.append(this.editOption);
        }
       }


       var accept = new Button({
         type: model.acceptType(),
         style : BrowserInfo.isSmallScreen() ? "margin-top:-10px" : "",
         cssClass: "float-right",
         text: this.model.acceptText(),
         onClick : function() {
           var customtext = mailview.customtext();
           var res = model.accept(customtext != undefined && customtext != "" ? customtext : undefined);
           if (res == true) view.reject(); //We don't actually reject. Just clean the modal.
         }
       });
       var acceptButton = accept.el();

       footer.append(acceptButton);
       container.append(header);
       container.append(body);
       container.append(footer);
       $(this.el).append(container);
       return this;
    },
    reject: function(){
        var self = this;
        $(this.el).removeClass("active");
        this.model.onReject()();
        setTimeout(function() {if (self.clear != undefined) self.clear()}, 600);
    },
    edit: function(){
		this.editOption.remove();
		this.model.mail().makeEditable();
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


window.ConfirmationWithEmail = {
    popup: function (args) {
          var model = new ConfirmationWithEmailModel(args);
          var overlay = $("<div class='modal'/>");
          if (args.cssClass != undefined)
            overlay.addClass(args.cssClass);
          var overlay_height = null;
          if (!BrowserInfo.isIE8orLower()) {
            // IE7 and IE8 don't push the document down,
            // $(document).height() still reports the old size
            // but for other browsers make the overlay big asap so it looks good immediately
            overlay.height($(document).height());
          }

          if (BrowserInfo.isPadDevice()) {
            //Pad devices have a different aproach to body.width
            //Check http://stackoverflow.com/questions/6695676/100-width-css-issue-only-on-mobile-safari
            // Note also that width of sign page is dynamic due to arrows
            overlay.css("min-width",$(document).width());
          }

          var view = new ConfirmationWithEmailView({model : model, el : overlay});
          $("body").append(overlay);
          var updateOverlay = function() {
            if (!$.contains(document, overlay[0])) {
              $(window).unbind('resize', updateOverlay);
            }

            var left = Math.floor(($(window).width() - (BrowserInfo.isSmallScreen() ? 980 : 800)) / 2);
            overlay.find('.modal-container').css("margin-left",left > 20 ? left : 20);

            if (BrowserInfo.isIE8orLower()) {
              // for IE7 and IE8 don't make the overlay big, append
              // it directly and calculate it's real size, so we can increase the
              // document+overlay size later
              overlay_height = overlay.height();
            }
            if (model.bottom()) {
              view.container.css("margin-top", window.innerHeight - view.container.height() - 100);
            }
            // Sometimes when the overlay pushes the document down,
            // we have to make sure that the overlay covers the whole doc.
            if (BrowserInfo.isIE8orLower()) {
              // increase the overlay size (see prev comments)
              overlay.height(overlay_height + $(document).height());
            } else {
              overlay.height($(document).height());
            }
          };
          $(window).resize(updateOverlay);
          setTimeout(function() {
            overlay.addClass("active");
            setTimeout(function() {
              updateOverlay();
            }, 600);
          }, 100);
          return model;
   }

};

});
