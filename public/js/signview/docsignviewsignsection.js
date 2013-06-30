/* Signatory view of document
 * Usage:
 *
 *   $('body').append(new DocumentSignSignSection(model : document).el);
 */


(function(window) {


window.DocumentSignConfirmation = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'popup');
    _.bindAll(this, 'createContentElems');
  },
  document : function() {
    return this.model.document();
  },
  createElegButtonElems: function() {
    var document = this.document();
    var signatory = document.currentSignatory();

    var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
    var telia = $("<a href='#' class='author2 telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
    var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
    var mbi = $("<a href='#' class='mbi'><img src='/img/mobilebankid.png' alt='Mobilt BankID' /></a>");
    bankid.click(function() {
      mixpanel.track('Click BankID');
      document.takeSigningScreenshot(function() { Eleg.bankidSign(document, signatory, function(p) { document.sign().addMany(p).sendAjax(); });});
      return false;
    });
    telia.click(function() {
      mixpanel.track('Click Telia');
      document.takeSigningScreenshot(function() { Eleg.teliaSign(document, signatory,  function(p) { document.sign().addMany(p).sendAjax(); });});
      return false;
    });
    nordea.click(function() {
      mixpanel.track('Click Nordea');
      document.takeSigningScreenshot(function() { Eleg.nordeaSign(document, signatory, function(p) { document.sign().addMany(p).sendAjax(); });});
      return false;
    });
    mbi.click(function() {
      mixpanel.track('Click Mobile BankID');
      document.takeSigningScreenshot(function() { Eleg.mobileBankIDSign(document,signatory,function(p) { document.sign().addMany(p).sendAjax(); },signatory.personalnumberField().value()); });
      return false;
    });
    return $("<span />").append(bankid).append(telia).append(nordea).append(mbi);
  },
  createSignButtonElems: function() {
    var document = this.document();
    var guardModel = this.guardModel;
    return Button.init({
      size: BrowserInfo.isSmallScreen() ? "big" : "small",
      color: "blue",
      text: document.process().processLocalization().signbuttontext,
      onClick: function() {
        if (alreadyClicked(this))
          return false;
        trackTimeout('Accept', {'Accept' : 'sign document'});
        var errorCallback = function(xhr) {
          if (xhr.status == 403) {
            // session timed out
            ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
          } else {
            window.location.reload();
          }
        };
        document.takeSigningScreenshot(function() { document.sign(errorCallback).send(); });
      }
    }).input().css('margin-top', '-10px')
              .css('margin-bottom', BrowserInfo.isSmallScreen() ? '10px' : '0px');
  },
  createPreambleElems: function() {
    var document = this.document();
    var signatory = document.currentSignatory();

    if (signatory.author) {
     var content = $("<div />");
     if (document.authorIsOnlySignatory())
            content = $(document.process().processLocalization().signatorysignmodalcontentauthoronly);
     else if (signatory.elegAuthentication())
          content.append(document.process().processLocalization().signatorysignmodalcontentsignvieweleg);
     else
          content.append(document.process().processLocalization().signatorysignmodalcontent);

     if (signatory.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.signByAuthor.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.signByAuthor.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.signByAuthor.eleg.body1).append(a).append(localization.signByAuthor.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    } else {
      var content = $("<div />");
      if (signatory.elegAuthentication())
          content.append(document.process().processLocalization().signatorysignmodalcontentsignvieweleg);
      else
          content.append(document.process().processLocalization().signatorysignmodalcontent);

      if (signatory.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    }
  },
  createContentElems: function() {
    var content = $("<div />");
    content.append(this.createPreambleElems());
    if (BrowserInfo.isSmallScreen()) {
        var p = content.find('p');
        p.css('font-size', '52px');
        p.css('line-height', '72px');
        p.css('margin-top', '40px');
    }
    return content;
  },
  popup: function() {
    var document = this.document();
    var signatory = document.currentSignatory();

    Confirmation.popup({
      title: signatory.author ? localization.signByAuthor.modalTitle : document.process().processLocalization().signatorysignmodaltitle,
      acceptButton: signatory.elegAuthentication() ? this.createElegButtonElems() : this.createSignButtonElems(),
      rejectText: localization.cancel,
      textcolor : this.model.usebranding() ? document.signviewtextcolour() : undefined,
      textfont : this.model.usebranding() ? document.signviewtextfont() : undefined,
      content: this.createContentElems
    });

    // Re-adjust the signing modal for small screen devices.
    if (BrowserInfo.isSmallScreen()) {
      // Remove the modal header but keep the close button
      var modalHeader = $('.modal-container .modal-header');
      var close = modalHeader.find('.modal-close').detach();
      modalHeader.remove();
      close.css('margin', '45px 475px 30px 30px');
      close.css('background', 'none');
      close.css('font-size', '55px');
      close.css('color', 'red');
      close.css('width', '420px');
      close.css('height', '90px');
      close.text(localization.goBack);

      // Remove the modal footer but keep the button
      var modalFooter = $('.modal-container .modal-footer');
      var signButton = modalFooter.find('.button').detach();
      if (signButton.length == 0) {
        // No sign button means bankid
        signButton = modalFooter.find('.mbi').addClass("button").detach().prepend('Signera');
        signButton.find('img').remove();
      }
      modalFooter.remove();

      $('.modal-container .modal-body .modal-content').css('border-bottom', '0px');

      signButton.css({
        'font-size': '100px',
        'height': '100px',
        'width': '80%',
        'max-height': '120px',
        'margin-right': '48px',
        'margin-bottom': '65px',
        'margin-top': '40px',
        'line-height': '105px',
        'padding-bottom': '55px',
        'padding-top': '50px'
      });

      $('.modal-container').append(signButton);
      $('.modal-container').append(close);
    }
  }
});

window.DocumentSignSignSection = Backbone.View.extend({
   initialize : function(args){
      this.render();
   },
   render: function() {
       var model = this.model;
       var document = this.model.document();
       var box = $(this.el).addClass('section').addClass('spacing').addClass('signbuttons');
       var signatory = document.currentSignatory();
       var sps = {};
       sps['Has user?'] = signatory.hasUser();
       sps['First visit'] = !signatory.seendate();
       mixpanel.register(sps);

       // track signatory properties
       var ps = {};
       ps['Full Name'] = signatory.nameOrEmail();
       ps['$email'] = signatory.email();
       if(signatory.fstname())
           ps['$first_name'] = signatory.fstname();
       if(signatory.sndname())
           ps['$last_name'] = signatory.sndname();
       if(signatory.hasUser())
           ps['$username'] = signatory.email();
       mixpanel.people.set(ps);

       mixpanel.track('View sign view');
       this.rejectButton = Button.init({
                                        size: BrowserInfo.isSmallScreen() ? 'big' : 'small',
                                        color: "red",
                                        shape : "rounded",
                                        width: 206,
                                        text: document.process().processLocalization().rejectbuttontext,
                                        onClick: function() {
                                            mixpanel.track('Click Reject');
                                            ConfirmationWithEmail.popup({
                                            title: document.process().processLocalization().signatorycancelmodaltitle,
                                            mail: document.currentSignatory().rejectMail(),
                                            acceptText: localization.reject.send,
                                            editText: localization.reject.editMessage,
                                            rejectText: localization.cancel,
                                            acceptColor: "red",
                                            textcolor : model.usebranding() ? document.signviewtextcolour() : undefined,
                                            textfont : model.usebranding() ? document.signviewtextfont() : undefined,
                                            onAccept: function(customtext) {
                                                if (alreadyClicked(this))
                                                  return;
                                                trackTimeout('Accept',
                                                             {'Accept' : 'reject document'},
                                                             function() {
                                                                 document.currentSignatory().reject(customtext).sendAjax(
                                                                   function() {window.location.reload();},
                                                                   function() {window.location.reload();}
                                                                );
                                                             });
                                              }
                                            });
                                        }
                                });
       this.signButton = Button.init({
                            size: BrowserInfo.isSmallScreen() ? 'big' : 'small',
                            shape : BrowserInfo.isSmallScreen() ? "" : "rounded",
                            color: "blue",
                            width: BrowserInfo.isSmallScreen() ?  404 : 206,
                            text: document.process().processLocalization().signbuttontext,
                            icon: BrowserInfo.isSmallScreen() ? undefined : $("<span class='icon cross' style='position: absolute; top: auto;margin-top: -1px;'></span>"),
                            onClick: function() {

                                var valid =  model.tasks().notCompleatedTasks().length == 1 && model.tasks().notCompleatedTasks()[0] == model.signtask();
                                if (!valid) {
                                        model.arrow().blink();
                                        return false;
                                    }
                                mixpanel.track('Click sign');
                                new DocumentSignConfirmation({
                                    model: model
                                    }).popup();
                                }
                            });

      var signButton = this.signButton.input();
      if (BrowserInfo.isSmallScreen()) {
        signButton.css({
          'padding-left': '33%',
          'padding-right': '33%',
          'font-size': '100px',
          'height': '100px',
          'max-height': '120px',
          'line-height': '85px',
          'padding-top': '55px',
          'padding-bottom': '55px'
        });
      }

      if (model.hasRejectOption() && !BrowserInfo.isSmallScreen()) {
        box.append($("<div class='rejectwrapper reject'>").append(this.rejectButton.input()));
        box.append($("<div class='signwrapper sign'>").append(signButton));
      }
      else {
        box.css("text-align","center").append($("<div class='signwrapper sign' style='width:100%;margin-right:0px;'>").append(signButton));
        if (BrowserInfo.isSmallScreen()) {
          box.css("padding", "0px");
        }
      }
      box.append($("<div class='clearfix' />"));

    setTimeout(function() {
      document.takeFirstScreenshot();
    }, 1500);
   }
});

})(window);
