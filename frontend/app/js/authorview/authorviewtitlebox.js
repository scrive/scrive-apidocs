define(['Backbone', 'legacy_code'], function() {

var AuthorViewTitleBoxModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  authorview :function() {
     return this.get("authorview");
  },
  document :function() {
     return this.authorview().document();
  },
  hasButtons : function() {
    return this.canBeRestarted() || this.canBeWithdrawn() || this.canGoToSignView();
  },
  canBeRestarted : function() {
    return (this.document().canberestarted() && this.document().currentViewerIsAuthor());
  },
  canBeProlonged : function() {
    return (this.document().canbeprolonged() && this.document().currentViewerIsAuthor());
  },
  restart : function(success) {
    this.document().restart().sendAjax(success);
  },
  canBeWithdrawn : function() {
    return this.document().canbecanceled() && (this.document().currentViewerIsAuthor() || this.document().currentViewerIsAuthorsCompanyAdmin());
  },
  cancel : function() {
    var self = this;
    LoadingDialog.open();
    this.document().cancel().sendAjax(function() {
      self.authorview().reload(true);
    });
  },
  canGoToSignView : function() {
    return this.document().currentViewerIsAuthor() && this.document().currentSignatoryCanSign() && this.document().pending();
  },
  canGiveToNextSignatoryPad : function() {
    return !this.canGoToSignView() && this.document().currentViewerIsAuthor() && this.document().pending() && _.any(this.document().signatoriesThatCanSignNow(), function(s) {return s.padDelivery();}) && this.document().signatoriesThatCanSignNow().length > 0;
  },
  goToSignView : function() {
    new Submit({method: 'POST', url : '/d/signview/' + this.document().documentid()}).send();
  },
  padNextSignatory : function() {
    if (this.get("padNextSignatory") != undefined) return this.get("padNextSignatory");
    return this.document().signatoriesThatCanSignNow()[0];
  },
  setPadNextSignatory : function(sig) {
    this.set("padNextSignatory",sig);
  },
  giveToPadSignatory : function() {
      if (this.padNextSignatory() != undefined)
        this.padNextSignatory().addtoPadQueue().sendAjax(function() {
                      window.location =  '/padqueue';
                });
  }
});

var AuthorViewTitleBoxView = Backbone.View.extend({
 initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.document().on('change', this.render);
    this.render();
  },
  destroy : function() {
    this.model.document().off('change', this.render);
    this.model.off();
    $(this.el).remove();
  },
  restartButton : function() {
    var model = this.model;
    var document = this.model.document();
    return new Button({
      color: "red",
      size: "big",
      shape: "rounded",
      text: localization.process.restartbuttontext,
      oneClick : true,
      onClick: function() {
        mixpanel.track('Click restart button');
        model.restart(function(resp) {
           var newdocdata = JSON.parse(resp);
           new FlashMessage({color: 'green', content : localization.flashDocumentRestarted, withRedirect : true, redirect : '/d/'+ newdocdata.id});
        });
      }
    }).el();
  },
  prolongButton : function() {
    var model = this.model;
    var document = this.model.document();
    return new Button({
      color: "green",
      size: "big",
      shape: "rounded",
      text: localization.process.prolongbuttontext,
      onClick: function() {
        mixpanel.track('Click prolong button');
        new ProlongModal({authorview : model.authorview()});
      }
    }).el();
  },
  withdrawnButton : function() {
    var model = this.model;
    var document = this.model.document();
    return new Button({
      size: "big",
      shape: "rounded",
      color: "red",
      text: localization.process.cancelbuttontext,
      cssClass: "s-withdraw-button",
      onClick: function() {
          mixpanel.track('Click withdraw button');
          var somebodysigned = _.any(document.signatories(),
                                      function(s) {
                                        return s.hasSigned() && !s.author();
                                      });
          var modalcontent = somebodysigned ? localization.process.cancelmodaltextwithsignatures : localization.process.cancelmodaltext;
        new Confirmation({
          title: localization.process.cancelmodaltitle,
          content: $('<p class="center">' + modalcontent + '</p>'),
          icon: '/img/modal-icons/withdraw.png',
          width: 533,
          acceptText: localization.process.cancelbuttontext,
          rejectText: localization.cancel,
          acceptColor: "green",
          extraClass : "s-withdraw-confirmation",
          onAccept: function() {
              trackTimeout('Accept',
                           {'Accept' : 'withdraw document'},
                           function() {
                               model.cancel();
                           });
              return true;
            }
          });
        }
    }).el();
  },
  goToSignViewButton : function() {
    var model = this.model;
    var document = this.model.document();
    return new Button({
      color: "green",
      size: "big",
      shape: "rounded",
      text: localization.authorview.goToSignView,
      onClick: function() {
          mixpanel.track('Click go to sign view');
         model.goToSignView();
        }
    }).el();
  },
  giveToNextSignatoryPadButton : function() {
    var self = this;
    var model = this.model;
    var document = this.model.document();
    var sig = document.signatoriesThatCanSignNow()[0];
    if (sig == undefined) return; // Should never happend
    return new Button({
      color: "green",
      size: "big",
      shape: "rounded",
      text: localization.authorview.goToSignView,
      onClick: function() {
          //Refactor this when we will get confirmation that this is what viktor needs.
          mixpanel.track('Give for pad signing to some pad signatory - opening modal');
          self.signatory = document.signatoriesThatCanSignNow()[0];
          var modalContent = function() {
            var div = $("<div style='height:32px;'/>");
            var label = $("<label style='float:left;padding-right:10px;line-height: 32px;'>").text(localization.pad.giveForSigningThisDevice + " " );
            div.append(label);
            if (document.signatoriesThatCanSignNowOnPad().length > 1)
            {
              var options = [];
              _.each(document.signatoriesThatCanSignNowOnPad(),function(sig) {
                if (sig != model.padNextSignatory())
                options.push({
                  name: (sig.smartname() != "" ? sig.smartname() : localization.pad.notNamedParty),
                  onSelect : function() {model.setPadNextSignatory(sig); return true;}
                });
              });
              var select = new Select({
                name : (model.padNextSignatory().smartname() != "" ? model.padNextSignatory().smartname() : localization.pad.notNamedParty),
                cssClass : "float-left",
                options : options
              });
              div.append(select.el());
            }
            else {
              label.append($("<strong/>").text(sig.smartname() != "" ? sig.smartname() : localization.pad.notNamedParty.toLowerCase()));
            }
            return div;

          };
          self.padNextSignatoryModalContent = modalContent();
          model.bind("change:padNextSignatory", function() {
            var c = modalContent();
            self.padNextSignatoryModalContent.replaceWith(c);
            self.padNextSignatoryModalContent = c;
          });
          new Confirmation({
            title : localization.authorview.goToSignView,
            content :self.padNextSignatoryModalContent,
            onAccept : function() {
                mixpanel.track('Give for pad signing to some pad signatory - opening signview');
                model.giveToPadSignatory(self.signatory);
            }
          });
          return false;
        }
    }).el();
  } ,
  render: function() {
    var document = this.model.document();
    $(this.el).empty();

    var container = $("<div class='titlebox' />");
    container.append($("<div class='headline' />").text( document.title() ));


    var smallerbits = $("<div class='subheadline'/>");
    container.append(smallerbits);

    smallerbits.append($("<a target='_blank' class='download clickable' />").attr("href", document.mainfile().downloadLinkForMainFile(document.title())).text(localization.authorview.downloadPdf));
      mixpanel.track_links('.download', 'Download PDF');

    $(this.el).append(container);

    if (this.model.hasButtons()) {
      console.log("Generating buttons");
      var buttonbox = $("<div class='buttonbox'/>");
      if (this.model.canBeRestarted())
        buttonbox.append(this.restartButton());
      if (this.model.canBeProlonged())
        buttonbox.append(this.prolongButton());
      if (this.model.canBeWithdrawn())
        buttonbox.append(this.withdrawnButton());
      if (this.model.canGoToSignView())
        buttonbox.append(this.goToSignViewButton());
      else if (this.model.canGiveToNextSignatoryPad())
        buttonbox.append(this.giveToNextSignatoryPadButton());
      container.append(buttonbox);
    }
    return this;
  }

});

window.AuthorViewTitleBox = function(args) {
          var model = new AuthorViewTitleBoxModel(args);
          var view =  new AuthorViewTitleBoxView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.destroy = function() { view.destroy();};

};

});
