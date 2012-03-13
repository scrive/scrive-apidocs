/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

window.DocumentStandarView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.model.view = this;
        this.prerender();
        this.render();
    },

    prerender: function(){
        this.contrainer = $("<div class='mainContainer' />");
        $(this.el).append(this.contrainer);
        $(this.el).addClass("body-container");
        $(this.el).append("<div class='clearfix'/>");
        $(this.el).append("<div class='spacer40'/>");

    },
    attachmentsTab : function() {
          var document = this.model;
          var attachmenttabview = $("<span id='attachmenttabview' />");
          var body = $("<div class='signStepsBody ericfix forauthor'/>");
          var attachmentlist=  $("<div class='attachmentListContainer'/>");
          if (document.authorattachments().length > 0 ) {
            var authorattachmentlist= $("<div class='float-left'/>");
            authorattachmentlist.append($("<div class='subheader'/>").text(localization.attachment));
            _.each(document.authorattachments(), function(file){
              var a = $("<a target='_blank'/>").attr("href", file.downloadLink()).text(file.name());
              authorattachmentlist.append($("<div/>").append(a));
            });
            attachmentlist.append(authorattachmentlist);
          }
          if (document.signatoryattachments().length > 0 ) {
            var signatoryattachmentlist = $("<div class='float-left' style='margin-left:20px;' />");
            signatoryattachmentlist.append($("<div class='subheader'/>").text(localization.requestedAttachments));
            var signatoryattachmenttable = $("<table class='authorsigattachment'/>");
            _.each(document.signatoryattachments(), function(attachment){

              var namerow = $("<td/>");
              if (attachment.file() != undefined)
                namerow.append($("<a target='_blank' style='margin-right:10px'/>").text(attachment.name()).attr("href", attachment.file().downloadLink()));
              else
                namerow.text(attachment.name());
              var sigrow  = $("<td>").text(attachment.signatory().name());
              signatoryattachmenttable.append($("<tr/>").append(namerow).append(sigrow));

            });
            signatoryattachmentlist.append(signatoryattachmenttable);
            attachmentlist.append(signatoryattachmentlist);
          }
         attachmenttabview.append(body.append(attachmentlist));
         return  attachmenttabview;
    },
    signatoriesTab : function(){
          var document = this.model;
          var signatoriestabview = $("<span id='documenttabview' />");
          var body = $("<div class='signStepsBody ericfix forauthor'/>");
          var firstbox = $("<div id='signViewBodyLeft' />");
          firstbox.append(document.infotext());
          firstbox.append("<BR/>");
          // Making restart button
          if (document.canberestarted())
          {
            var restartbutton =  Button.init({
                    color: "red",
                    size:   "small",
                    text:  document.process().restartbuttontext(),
                    onClick : function(){ document.restart().send(); }
            });
            firstbox.append("<BR/>");
            firstbox.append(restartbutton.input());
          }
          // Making cancel button
           if (document.canbecanceled())
           {
             firstbox.append("<BR/>");
             firstbox.append(this.cancelButton().input());
          }

          var middlebox = $("<div class='float-left signViewBodyBox'/>");
          if (document.currentSignatory() != undefined)
          {
              if (document.currentSignatory().author() || document.currentSignatory().signs()) {
                var currentsignatoryview = new SignatoryStandarView({model : document.currentSignatory(), el : $("<div/>")});
                middlebox.append($(currentsignatoryview.el));
              }
          }

          var lastbox = $("<div class='float-right signViewBodyBox'/>");
          var othersignatories = document.otherSignatories();
          _.each(document.otherSignatories(), function(signatory) {
              if (signatory.author() || signatory.signs()) {
                var signatoryview = new SignatoryStandarView({model : signatory, el : $("<div/>")});
                lastbox.append($(signatoryview.el));
              }
          });


          body.append(firstbox);
          body.append(middlebox);
          body.append(lastbox);
          signatoriestabview.append(body);
          return signatoriestabview;

    },
    cancelButton : function() {
          var document = this.model;
          return Button.init({
                    color: "red",
                    size:   "small",
                    text:  document.process().cancelbuttontext(),
                    onClick : function(){
                         Confirmation.popup({
                            title : document.process().cancelmodaltitle(),
                            content : document.process().cancelmodaltext(),
                            acceptText : document.process().cancelbuttontext(),
                            rejectText : localization.cancel,
                            acceptColor : "red",
                            onAccept : function()
                            {
                                document.cancel().send();
                                return true;
                            }
                            });
                        }
                });
    },
    authorAttachmentBox : function() {
      var document = this.model;
      var signatory = document.currentSignatory();
      var box = $("<div id='signViewAttachmentBox'/>");
      var header = $("<h2/>").text(localization.authorAttachmentBoxHeader);
      var list = $("<div id='signViewAttachmentBoxList'/>");
         _.each(document.authorattachments(), function (attachment) {
           var a = $("<a target='_blank'/>").text(attachment.name());
           a.attr("href", attachment.downloadLink());
           list.append($("<div>").append(a));
        });
      box.append(header);
      box.append(list);
      return box;
    },
    signatoryAttachmentBox : function() {
      var document = this.model;
      var signatory = document.currentSignatory();
      var box = $("<div id='signViewSigAttachmentBox'/>");
      var header = $("<h2/>").text(localization.signatoryAttachmentBoxHeader);

      var list = $("<table id='signViewSigAttachmentBoxList'/>");
      var lbody = $("<tbody>");
      list.append(lbody);
         _.each(signatory.attachments(), function (attachment) {
           var view = new SignatoryAttachmentRowView({model : attachment, el : $("<tr/>")});
           lbody.append($(view.el));
        });
      box.append(header);
      box.append(list);


      return box;
    },
    signBox: function() {
      var document = this.model;
      var signatory = document.currentSignatory();
      var box = $("<div id='signViewBottomBox'/>");
      var leftbox = $("<div id='signViewBottomBoxContainerLeft'/>");
      var cancelButton = Button.init({
                                size:"small",
                                color: "red",
                                text: document.process().rejectbuttontext(),
                                style : "width: 150px;margin-right: 12px;",
                                onClick : function(){
                                    ConfirmationWithEmail.popup({
                                        title : document.process().signatorycancelmodaltitle(),
                                        mail : signatory.rejectMail(),
                                        acceptText : localization.reject.send,
                                        editText :  localization.reject.editMessage,
                                        rejectText : localization.cancel,
                                        acceptColor : "red",
                                        onAccept : function(customtext)
                                        {
                                            signatory.reject(customtext).send();
                                        }
                                    });
                        }
                            });
      leftbox.append(cancelButton.input());
      box.append (leftbox);
      var guardbox;
      var checkbox;
        console.log("xxx");
      if (document.process().requiressignguard())
      {
          console.log("requires sign guard");
          var middlebox = $("<div id='signViewBottomBoxContainerMiddle'/>");
          guardbox = $("<div id='signGuardField'/>");
          var checkboxwrapper =  $("<div class='float-left'/>");
          checkbox =  $("<input type='checkbox'  id='signGuardCBox' class='signGuard' autocomplete='off'/>");
          checkbox.click(function() {guardbox.css("border","");});
          checkboxwrapper.append(checkbox);
          guardbox.append(checkboxwrapper);
          var labelwrapper =  $("<div class='float-left' id='signGuardLabel'/>");
          var label =  $("<label for='signGuardCBox'/>").append(localization.sign.guardCBox);
          labelwrapper.append(label);
          guardbox.append(labelwrapper);
          middlebox.append(guardbox);
          box.append(middlebox);


      }

      var rightbox = $("<div id='signViewBottomBoxContainerRight'/>");
      var acceptButton = Button.init({
                                size:"big",
                                color: "blue",
                                text: document.process().signbuttontext(),
                                icon : jQuery("<span class='icon cross' style='margin-left:5px'></span>"),
                                labelstyle : document.process().requiressignguard()? "width:120px" : "padding-left:20px;padding-right:20px;",
                                cssClass : "center" ,
                                onClick : function() {
                                    if(!signatory.signatureReadyForSign()) {
                                     signatory.signature().trigger('empty');
                                     FlashMessages.add({content: localization.signature.missing, color: "red"});
                                    }
                                    
                                    else if(!signatory.allFieldsReadyForSign()) {
                                     _.each(signatory.fields(),function(field) {
                                            if (!field.readyForSign() && field.view != undefined)
                                                field.view.redborder();
                                        });
                                        FlashMessages.add({content: localization.mustFillFieldsBeforeSigning, color: "red"});
                                    }
                                    else if (!signatory.allAttachemntHaveFile())
                                    {
                                        _.each(signatory.attachments(),function(attachment) {
                                            if (!attachment.hasFile())
                                                $(attachment.view.el).addClass("redborder");
                                        });
                                        FlashMessages.add({content: localization.addRequiredAttachments, color: "red"});
                                    }
                                    else if (checkbox != null && !checkbox.is(":checked"))
                                    {
                                        guardbox.css("border", "1px dotted red");
                                        FlashMessages.add({content: document.process().signguardwarntext(), color: "red"});
                                        return;
                                    }
                                    else
                                        document.view.popupSignConfirmation();
                                }
                            });
      rightbox.append(acceptButton.input());
      box.append(rightbox);

      return box;
    },
    popupSignConfirmation : function() {
        var document = this.model;
        var signatory = document.currentSignatory();
        var acceptButton;
        if (document.elegAuthorization())
        {
            acceptButton = $("<span/>");
            var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
            var telia = $("<a href='#' class='author2 telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
            var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
            bankid.click(function() {
                    Eleg.bankidSign(document,signatory, document.sign());
                    return false;
            });
            telia.click(function() {
                    Eleg.teliaSign(document,signatory, document.sign());
                    return false;
            });
            nordea.click(function() {
                    Eleg.nordeaSign(document,signatory, document.sign());
                    return false;
            });
            acceptButton.append(bankid).append(telia).append(nordea);
        }
        else
        {
            acceptButton = Button.init({
                  size: "small",
                  color : "blue",
                  icon : $("<span class='btn-symbol cross' style='margin-left: 10px'/>"),
                  text : document.process().signbuttontext(),
                  onClick : function() {
                      document.sign().send();
                    }
                }).input();
        }
        var content = document.lastSignatoryLeft() ? $(document.process().signatorysignmodalcontentlast()) : $(document.process().signatorysignmodalcontentnotlast());
        DocumentDataFiller.fill(document, content);
        if (document.elegAuthorization())
        {

            var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
            var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
            var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
            content = content.add($("<span/>").append(subhead).append(p));
        }
        Confirmation.popup({
              title : document.process().signatorysignmodaltitle(),
              acceptButton : acceptButton,
              rejectText: localization.cancel,
              content  : content
        });

    },
    popupSignConfirmationByAuthor : function() {
        var document = this.model;
        var signatory = document.currentSignatory();
        var content = document.lastSignatoryLeft() ? $(document.process().signatorysignmodalcontentauthorlast()) : $(document.process().signatorysignmodalcontentnotlast());
        var acceptButton;
        if (document.elegAuthorization())
        {
            acceptButton = $("<span/>");
            var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
            var telia = $("<a href='#' class='telia'><img src='/img/telia.png' alt='Telia Eleg' /></a>");
            var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
            bankid.click(function() {
                    Eleg.bankidSign(document,signatory, document.sign());
                    return false;
            });
            telia.click(function() {
                    Eleg.teliaSign(document,signatory, document.sign());
                    return false;
            });
            nordea.click(function() {
                    Eleg.nordeaSign(document,signatory, document.sign());
                    return false;
            });
            acceptButton.append(bankid).append(telia).append(nordea);
        }
        else
        {
            acceptButton =
                Button.init({
                  size: "small",
                  color : "blue",
                  icon : $("<span class='btn-symbol cross' style='margin-left: 10px'/>"),
                    text :  document.process().authorsignlastbutton(),
                  onClick : function() {
                      document.sign().send();
                    }
                }).input();
        }

       if (document.elegAuthorization())
        {

            var subhead = $("<h3/>").text(localization.signByAuthor.eleg.subhead);
            var a = $("<a target='_new' />").text(localization.signByAuthor.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
            var p = $("<p/>").append(localization.signByAuthor.eleg.body1).append(a).append(localization.signByAuthor.eleg.body2);
            content.add($("<span/>").append(subhead).append(p));
        }

        Confirmation.popup({
              title : localization.signByAuthor.modalTitle,
              acceptButton : acceptButton,
              rejectText: localization.cancel,
              content  : content
        });

    },
    signForAuthorBox : function() {
      var document = this.model;
      var signatory = document.currentSignatory();
      var box = $("<div id='signViewBottomBox'/>")   ;
      var leftbox = $("<div id='signViewBottomBoxContainerLeft'/>") ;
      leftbox.append(this.cancelButton().input());
      box.append (leftbox);

        if (document.process().requiressignguard())
        {
            var middlebox = $("<div id='signViewBottomBoxContainerMiddle'/>");
            console.log("requires sign guard");

            var guardbox = $("<div id='signGuardField'/>");
            var checkboxwrapper =  $("<div class='float-left'/>");
            var checkbox =  $("<input type='checkbox'  id='signGuardCBox' class='signGuard' autocomplete='off'/>");
            checkbox.click(function() {guardbox.css("border","");});
            checkboxwrapper.append(checkbox);
            guardbox.append(checkboxwrapper);
            var labelwrapper =  $("<div class='float-left' id='signGuardLabel'/>");
            var label =  $("<label for='signGuardCBox'/>").append(localization.signByAuthor.guardCBoxForAuthorLabel);
            labelwrapper.append(label);
            guardbox.append(labelwrapper);
            guardbox.append(localization.signByAuthor.guardCBoxForAuthorAfterLabel);
            middlebox.append(guardbox);
            box.append(middlebox);
        }
      var rightbox = $("<div id='signViewBottomBoxContainerRight'/>");
      var acceptButton = Button.init({
                                size:"big",
                                color: "blue",
          text: document.process().authorsignlastbutton(),
                                icon : jQuery("<span class='icon cross' style='margin-left:5px'/>"),
                                labelstyle :  "width:120px" ,
                                cssClass : "center" ,
                                onClick : function() {

                                    // We check only this since author probably does not need to have any fields or attachemnts
                                    if (checkbox != null && !checkbox.is(":checked"))
                                    {
                                        guardbox.css("border", "1px dotted red");
                                        FlashMessages.add({content: document.process().signguardwarntext(), color: "red"});
                                        return;
                                    }
                                    else
                                        document.view.popupSignConfirmationByAuthor();
                                }
                            });
      rightbox.append(acceptButton.input());
      box.append(rightbox);

      return box;
    },
    render: function () {
        var document = this.model;
        if (!document.ready())
            return this;
        this.contrainer.empty();
        /* Make title row */

        var titlepart = $("<span id='signStepsTitleRowTextContainer'/>");
        titlepart.append($("<span class='title'/>").text(document.process().title() + " #" + document.documentid() + ": "));
        titlepart.append($("<span class='name'/>").text(document.title()));
        var downloadpart = $("<span class='download'/>");
         if (document.mainfile() != undefined) {
            downloadpart.append($("<a  target='_blank'/>").attr("href",document.mainfile().downloadLink()).text(localization.downloadPDF));
         }

        /* Make file part */
        var bottomparts = $("<div/>");
        // Author attachment box
        if (document.currentSignatory() != undefined &&
            document.currentSignatory().signs() &&
            !document.currentSignatory().hasSigned() &&
            !document.currentSignatory().author() &&
            document.authorattachments().length > 0 &&
            document.signingInProcess())
        {
          bottomparts.append(this.authorAttachmentBox()) ;
        }
        // Signatory attachment box
        if (document.currentSignatory() != undefined &&
            document.currentSignatory().signs() &&
            !document.currentSignatory().hasSigned() &&
            document.currentSignatory().attachments().length > 0 &&
            document.signingInProcess())
        {
          bottomparts.append(this.signatoryAttachmentBox()) ;
        }

        // Sign boxes
        if (!document.currentViewerIsAuthor()
            && document.currentSignatory() != undefined
            && document.currentSignatory().canSign())
        {
          bottomparts.append(this.signBox()) ;
        }
        else if (document.currentViewerIsAuthor() && document.awaitingauthor())
        {
          bottomparts.append(this.signForAuthorBox()) ;
        }

        var file = KontraFile.init({file: document.mainfile(), document : document});
        var tabs = KontraTabs.init({
            title : jQuery.merge(titlepart,downloadpart),
            tabs: [
                new Tab({
                    name : localization.document,
                    elems: [
                            this.signatoriesTab(),
                            $(file.view.el),
                            bottomparts
                           ]
                  }),
                new Tab({
                    name  : localization.attachmentsWord,
                    elems : [
                            this.attachmentsTab(),
                            $(file.view.el),
                            bottomparts
                            ],
                    disabled : !this.model.hasAnyAttachments() ||
                               ( document.currentSignatory() != undefined &&
                                 document.currentSignatory().signs() &&
                                 !document.currentSignatory().hasSigned() &&
                                 document.signingInProcess() &&
                                 !document.currentSignatory().author()
                               )
                  })
                ]
        });
        this.contrainer.append($(tabs.view.el));



        return this;

    }
});

window.KontraStandardDocument = {
    init : function(args){
       this.model = new Document({
                        id : args.id,
                        viewer : args.viewer
                    });
       this.view = new DocumentStandarView({
                        model: this.model,
                        el : $("<div/>")
                    });
       this.recall();
       return this;
   },
   recall : function()
   {
       this.model.recall();
   }
};
})(window);
