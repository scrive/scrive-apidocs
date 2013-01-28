(function(window){

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
  restart : function() {
    this.document().restart().send();
  },
  canBeWithdrawn : function() {
    return this.document().canbecanceled() && (this.document().currentViewerIsAuthor() || this.document().currentViewerIsAuthorsCompanyAdmin());
  },
  cancel : function() {
    this.document().cancel().sendAjax(function() {window.location = window.location;});
  },
  canGoToSignView : function() {
    return this.document().currentViewerIsAuthor() && this.document().currentSignatoryCanSign();
  },
  canGiveToNextSignatoryPad : function() {
    return !this.canGoToSignView() && this.document().currentViewerIsAuthor() && this.document().padDelivery() && this.document().signatoriesThatCanSignNow().length > 0;
  },
  goToSignView : function() {
    new Submit({method: 'POST', url : '/d/signview/' + this.document().documentid()}).send();
  },
  padNextSignatory : function() {
    if (this.get("padNextSignatory") != undefined) return this.get("padNextSignatory");
    return this.document().signatoriesThatCanSignNow()[0]
  },
  setPadNextSignatory : function(sig) {
    this.set("padNextSignatory",sig)
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
    this.model.document().bind('change', this.render);
    this.render();
  },
  // Big instruction or information about document state
  text: function() {
    return this.model.document().title();
  },
  // Smaller text with more details on some states
  dueDateDescription : function() {
      var timeout = this.model.document().timeouttime();
      var timeoutText = moment(timeout).format("YYYY-MM-DD");
      return localization.docsignview.dueDate + " " + timeoutText;
  },
  restartButton : function() {
    var model = this.model;
    var document = this.model.document();
    return Button.init({
      color: "red",
      size: "small",
      text: document.process().localization().restartbuttontext,
      onClick: function() {
        if (alreadyClicked(this))
          return;
          mixpanel.track('Click restart button');
        model.restart();
      }
    }).input();
  },
  withdrawnButton : function() {
    var model = this.model;
    var document = this.model.document();
    return Button.init({
      color: "red",
      size: "small",
      text: document.process().localization().cancelbuttontext,
      cssClass: "s-withdraw-button",
      onClick: function() {
          mixpanel.track('Click withdraw button');
        Confirmation.popup({
          title: document.process().localization().cancelmodaltitle,
          content: document.process().localization().cancelmodaltext,
          acceptText: document.process().localization().cancelbuttontext,
          rejectText: localization.cancel,
          acceptColor: "red",
          extraClass : "s-withdraw-confirmation",
          onAccept: function() {
              if (alreadyClicked(this))
                return;
              trackTimeout('Accept',
                           {'Accept' : 'withdraw document'},
                           function() {
                               model.cancel();
                           });
              return true;
            }
          });
        }
    }).input();
  },
  goToSignViewButton : function() {
    var model = this.model;
    var document = this.model.document();
    return Button.init({
      color: "green",
      size: "small",
      text: localization.authorview.goToSignView,
      onClick: function() {
          mixpanel.track('Click go to sign view');
         model.goToSignView();
        }
    }).input();
  },
  giveToNextSignatoryPadButton : function() {
    var self = this;
    var model = this.model;
    var document = this.model.document();
    var sig = document.signatoriesThatCanSignNow()[0];
    if (sig == undefined) return; // Should never happend
    return Button.init({
      color: "green",
      size: "small",
      text: localization.authorview.goToSignView,
      onClick: function() {
          //Refactor this when we will get confirmation that this is what viktor needs.
          mixpanel.track('Give for pad signing to some pad signatory - opening modal');
          self.signatory = document.signatoriesThatCanSignNow()[0];
          var modalContent = function() {
            var div = $("<div style='height:32px;'/>");
            div.append($("<label style='float:left;padding-right:10px;line-height: 32px;'>").text(localization.pad.giveForSigningThisDevice + " " ));
            var options = [];
            _.each(document.signatoriesThatCanSignNow(),function(sig) {
               if (sig != model.padNextSignatory())
               options.push({
                 name: (sig.smartname() != "" ? sig.smartname() : localization.pad.notNamedParty),
                 onSelect : function() {model.setPadNextSignatory(sig);}
              });
            })
            var select = new Select({
              name : (model.padNextSignatory().smartname() != "" ? model.padNextSignatory().smartname() : localization.pad.notNamedParty),
              textWidth : 160,
              options : options
            });
            return div.append($(select.view().el).css("width","200px").addClass('float-left'));
          }
          self.padNextSignatoryModalContent = modalContent();
          model.bind("change:padNextSignatory", function() {
            var c = modalContent();
            self.padNextSignatoryModalContent.replaceWith(c);
            self.padNextSignatoryModalContent = c;
          })
          Confirmation.popup({
            title : localization.authorview.goToSignView,
            content :self.padNextSignatoryModalContent,
            onAccept : function() {
                mixpanel.track('Give for pad signing to some pad signatory - opening signview');
                model.giveToPadSignatory(self.signatory);
            }
          });
          return false;
        }
    }).input();
  } ,
  render: function() {
    var document = this.model.document();
    $(this.el).empty();

    var container = $("<div class='titlebox' />");
    container.append($("<div class='headline' />").text(this.text()));


    var smallerbits = $("<div class='subheadline'/>");
    container.append(smallerbits);
    if (document.timeouttime() != undefined && document.signingInProcess())
      smallerbits.append($("<label class='duedate'/>").text(this.dueDateDescription()));
    smallerbits.append($("<a target='_blank' class='download clickable' />").attr("href", document.mainfile().downloadLinkForMainFile(document.title())).text(localization.downloadPDF));
      mixpanel.track_links('.download', 'Download PDF');

    $(this.el).append(container);

    if (this.model.hasButtons()) {
      var buttonbox = $("<div class='buttonbox'/>");
      if (this.model.canBeRestarted())
        buttonbox.append(this.restartButton());
      if (this.model.canBeWithdrawn())
        buttonbox.append(this.withdrawnButton());
      if (this.model.canGoToSignView())
        buttonbox.append(this.goToSignViewButton());
      else if (this.model.canGiveToNextSignatoryPad())
        buttonbox.append(this.giveToNextSignatoryPadButton())
      container.append(buttonbox);
    }
    return this;
  }

});

window.AuthorViewTitleBox = function(args) {
          var model = new AuthorViewTitleBoxModel(args);
          var view =  new AuthorViewTitleBoxView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);
