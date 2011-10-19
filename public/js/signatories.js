/* Signatories model + basic view + signatories attachments
 */


(function(window){

window.SignatoryAttachment = Backbone.Model.extend({
    defaults: {
        name : "",
        description : ""
    },
    initialize: function(args) {
      if (args.file != undefined)
        this.set({"file" : new File( _.extend(args.file,{document : args.signatory.document() })) });
      return this;
    },
    file : function() {
        return this.get("file");
    },
    description : function() {
        return this.get("description");
    },
    name : function() {
        return this.get("name");
    },
    hasFile: function(){
        return this.file() != undefined;
    },
    signatory : function() {
        return this.get("signatory");
    }
});


// Note that this is done on tr
window.SignatoryAttachmentRowView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    render : function(){
           var attachment = this.model;
           var row = this.el;
           row.append($("<td/>").text(attachment.name()));
           row.append($("<td/>").text(attachment.description()));
           var lasttd = $("<td/>");
           if (attachment.hasFile())
           {  var filelink = $("<a target='_blank'/>").text(attachment.file().name()).attr("href",attachment.file().downloadLink());
              var removelink = $("<a href='' style='padding-left: 2em'>x</a>").click(function(){
                   new Submit({
                                method : "POST",
                                deletesigattachment : attachment.file().fileid()
                            }).send();
                   return false;         
                    });
              lasttd.append(filelink);
              lasttd.append(removelink);
            }
           else    
           {
                var uploadbutton = UploadButton.init({
                   width: 200,
                   name: "sigattach",
                   text: localization.signatoryAttachmentUploadButton,
                   submitOnUpload : true,
                   submit : new Submit({
                                method : "POST",
                                attachname : attachment.name(),
                                sigattachment : "YES"
                            }) 
                    
                });
                lasttd.append(uploadbutton.input());

            } 
           row.append(lasttd);
           return this;
 
    }
});    

window.Signatory = Backbone.Model.extend({
    defaults: {
        id : 0,
        signed : false,
        signs : false,
        author : false,
        fields : [],
        current : false,
        attachments : []
    },
    initialize : function(args){
        var signatory = this;
        var extendedWithSignatory =   function(hash){
                    hash.signatory = signatory;
                    return hash;
        };
        var fields =  _.map(args.fields, function(field){
                return new Field(extendedWithSignatory(field));
        });
        var attachments =  _.map(args.attachments, function(attachment){
                return new SignatoryAttachment(extendedWithSignatory(attachment));
        });
        this.set({"fields": fields ,
                  "attachments" : attachments
        });
    },
    document : function(){
        return this.get("document");
    },
    signatoryid : function(){
        return this.get("id");
    },
    author: function(){
        return this.get("author");
    },
    current: function(){
        return this.get("current");
    },
    status : function() {
        return this.get("status");
    },
    fields:  function(){
        return this.get("fields");
    },
    field : function(name) {
        var fields = this.fields();
        for(var i =0 ;i< fields.length; i++)
            if (fields[i].name() == name)
                return fields[i];
    },
    email: function(){
        return  this.field("email").value();
    },
    name : function(){
        var name = this.field("fstname").value() + " "+this.field("sndname").value();
        if (name != undefined)
            return name;
        else
            return "";                                    
    },
    smartname : function() {
        if (this.current()) 
         return localization.you;
        if (this.name() != "")
         return this.name();
        else 
         return this.email();
    },
    personalnumber : function() {
        var pn = this.field("sigpersnr").value();
        if (pn != undefined)
            return pn; 
        else
            return "";
    },
    signdate:function(){
        return  this.get("signdate");
    },
    datamismatch:function(){
        return  this.get("datamismatch");
    },
    rejecteddate:function(){
        return  this.get("rejecteddate");
    },
    seendate:function(){
        return  this.get("seendate");
    },
    readdate:function(){
        return  this.get("readdate");
    },
    deliveredEmail:function(){
        return  this.get("deliveredEmail");
    },
    undeliveredEmail: function() {
          return this.get("undeliveredEmail");
    },
    signorder : function() {
         return this.get("signorder");
    },
    signs : function() {
         return this.get("signs");
    },
	hasSigned: function() {
		return this.signdate() != undefined;
	},
    attachments: function() {
        return this.get("attachments");
    },
    canSign : function() {
        var canSign = this.document().pending() &&  this.signs() &&  !this.hasSigned() &&
                            this.signorder() == this.document().signorder();
        return canSign;
    },
    allAttachemntHaveFile : function() {
        return _.all(this.attachments(),function(attachment){
            return attachment.hasFile()
        })      
    },
    allFieldsReadyForSign: function() {
        return _.all(this.fields(),function(field){
            return field.readyForSign();
        })      
    },
	remind: function(customtext) {
        return new Submit({
              url: "/resend/" + this.document().documentid() + "/" + this.signatoryid(),
              method: "POST",
              customtext : customtext
          });
    },
    reject: function(customtext) {
        return new Submit({
              url: "/s/" + this.document().documentid() + this.document().viewer().urlPart(),
              method: "POST",
              customtext : customtext,
              cancel : "YES"
          });
    },
    
	changeEmail: function(email) {
		return new Submit({
                url: "/changeemail/" + this.document().documentid() + "/" + this.signatoryid(),
                method: "POST",
                email : email
         })
	},
	remindMail: function() {
		return new Mail({
						document: this.document(),
						signatory: this,
						type: "remind"
					   })
	},
	rejectMail: function() {
        return new Mail({
                        document: this.document(),
                        signatory: this,
                        type: "reject"
                       })
    }
})

window.SignatoryStandarView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.render();
    },
    signatorySummary : function(){
          var signatory = this.model;
          var document = signatory.document();
          if (signatory.signdate() != undefined)
               return localization.signatoryMessage.signed + " "+  signatory.signdate();
          else if (signatory.datamismatch() == true)
               return localization.signatoryMessage.datamismatch
          else if (document.timedout())
               return localization.signatoryMessage.timedout
          else if(document.canceled())
               return localization.signatoryMessage.cancelled
          else if (document.datamismatch())
               return " "
          else if (signatory.rejecteddate()!= undefined)
               return localization.signatoryMessage.rejected + " " + signatory.rejecteddate()
          else if (signatory.seendate()!= undefined)
               return localization.signatoryMessage.seen + " " + signatory.seendate()
          else if (signatory.readdate()!= undefined)
               return localization.signatoryMessage.read + " " + signatory.readdate()
          else if (signatory.deliveredEmail())
               return localization.signatoryMessage.delivered
          else
              return localization.signatoryMessage.other       
    },
    changeEmailOption : function(){
        var signatory = this.model;
        var container = $("<div style='margin-top: 10px'/>");
        var fstbutton = Button.init({
                            size: "tiny",
                            color: "blue",
                            text : localization.send,
                            onClick : function() {
                                container.empty();
                                var inputwrapper = $("<div class='field float-left' style='width:150px'/>")
                                var input = $("<input type='text' class='fieldvalue' />");
                                input.val(signatory.email())
                                var sndbutton = Button.init({
                                    cssClass : "float-right",
                                    size: "tiny",
                                    color: "blue",
                                    text : localization.send,
                                    onClick : function() { signatory.changeEmail(input.val()).send(); }
                                    })
                                inputwrapper.append(input);
                                container.append(inputwrapper);
                                container.append(sndbutton.input());
                                return false;
                             }
                          })
        container.append(fstbutton.input());
        return container;
    },
	remidenMailOption : function() {
		 var signatory = this.model;
		 var button = $("<a  class='btn-tiny green prepareToSendReminderMail'/>")
		 var icon = $("<div/>").addClass(signatory.hasSigned()? "reminderForSignedIcon": "reminderForSendIcon");
		 var text = signatory.hasSigned() ? signatory.document().process().remindagainbuttontext() : localization.reminder.send ;
		 var textbox = $("<div class='sendLinkText'/>").text(text);
		 button.append(icon).append(textbox);
		 button.click(function() {
			 ConfirmationWithEmail.popup({
				title : signatory.hasSigned() ? signatory.document().process().remindagainbuttontext() : localization.reminder.formHead, 
				mail : signatory.remindMail(),
				acceptText : signatory.hasSigned() ? localization.send : localization.reminder.formSend ,
				editText :  localization.reminder.formOwnMessage,
				rejectText : localization.cancel,
				onAccept : function(customtext)
					{
						 signatory.remind(customtext).send();
					}
			})
		 })
		 return button;
		
	},
    render: function(){
        var signatory = this.model;
        this.el.addClass("signViewBodyRight");
        var container = $("<div class='signViewBodyRightTextContainer'/>");
        this.el.append(container);
        var header = $("<div class='header'/>").text(signatory.name());
        container.append(header);
        var fieldsbox = $("<div class='signViewBodyForms'/>")
        _.each(signatory.fields(),function(field){
            if (field.name() == "fstname" ||
                field.name() == "sndname" ||
                field.name() == "email")
            return;
            if (field.canBeIgnored())
            return;    
            var fieldview = new FieldStandardView(
            { model : field,
              el : $("<div/>")
            });
            fieldsbox.append(fieldview.el);
        });
        container.append(fieldsbox);
        var emailview = new FieldStandardView(
            { model : signatory.field("email"),
              el : $("<div/>")                                
            });
        container.append(emailview.el);

        var textsummary = $("<div class='text'/>");
        if (signatory.signs()) {
            textsummary.append($("<div class='icon status'/>").addClass(signatory.status()));
            if (signatory.undeliveredEmail()) textsummary.append("<span style='color:#000000;position:relative;left:-4px;'>!</span> ");
            textsummary.append($("<span class='textstatus'/>").text(this.signatorySummary()));
        } 
        else {
            textsummary.text(signatory.document().process().authorissecretarytext());
        }
        container.append(textsummary);

        if (signatory.document().currentViewerIsAuthor() 
            && !signatory.author() 
            && ((signatory.document().pending() && signatory.canSign())
                || signatory.document().closed()))
		  container.append(this.remidenMailOption());
		
        if (signatory.undeliveredEmail() && signatory.document().currentViewerIsAuthor() && signatory.document().pending())
          container.append(this.changeEmailOption());
        
        return this;
    }
})
    
})(window); 
