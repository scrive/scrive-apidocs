var Backbone = require("backbone");
var React = require("react");
var Select = require("../../scripts/common/select");
var LoadingDialog = require("../loading.js").LoadingDialog;
var FlashMessage = require("../flashmessages.js").FlashMessage;
var LocalStorage = require("../storage.js").LocalStorage;
var Submit = require("../submits.js").Submit;
var _ = require("underscore");
var $ = require("jquery");
var Button = require("../buttons.js").Button;
var ProlongModal = require("./prolongmodal.js").ProlongModal;
var Confirmation = require("../confirmations.js").Confirmation;
var trackTimeout = require("../../scripts/common/track_timeout");


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
  canBeRestarted : function() {
    return ((this.document().timedout() || this.document().canceled() || this.document().rejected()) && this.document().currentViewerIsAuthor());
  },
  canBeProlonged : function() {
    return (this.document().timedout() && this.document().currentViewerIsAuthor());
  },
  restart : function(success) {
    this.document().restart().sendAjax(success);
  },
  canBeWithdrawn : function() {
    return this.document().pending() && (this.document().currentViewerIsAuthor() || this.document().currentViewerIsAuthorsCompanyAdmin());
  },
  cancel : function() {
    var self = this;
    LoadingDialog.open();
    var errorcallback = function() {
      new FlashMessage({content: localization.cannotWithdrawAlreadySignedDocument, type: 'error'});
      self.authorview().reload(true);
    };
    this.document().cancel().sendAjax(function() {
      self.authorview().reload(true);
    }, errorcallback);
  },
  canGoToSignView : function() {
    return this.document().currentSignatoryCanSign() && this.document().pending();
  },
  canGiveToNextSignatoryPad : function() {
    return !this.canGoToSignView() && this.document().currentViewerIsAuthor() && this.document().pending() && this.document().signatoriesThatCanSignNowOnPad().length > 0;
  },
  goToSignView : function() {
    LocalStorage.set("backlink", "target", "document");
    new Submit({method: 'GET', url : '/d/signview/' + this.document().documentid()}).send();
  },
  padNextSignatory : function() {
    if (this.get("padNextSignatory") != undefined) return this.get("padNextSignatory");
    return this.document().signatoriesThatCanSignNowOnPad()[0];
  },
  setPadNextSignatory : function(sig) {
    this.set("padNextSignatory",sig);
  },
  giveToPadSignatory : function() {
      if (this.padNextSignatory() != undefined)
        this.padNextSignatory().giveForPadSigning().send();
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
      type: "cancel",
      size: "big",
      text: localization.process.restartbuttontext,
      oneClick : true,
      onClick: function() {
        mixpanel.track('Click restart button');
        model.restart(function(resp) {
           var newdocdata = JSON.parse(resp);
           new FlashMessage({type: 'success', content : localization.flashDocumentRestarted, withRedirect : true, redirect : '/d/'+ newdocdata.id});
        });
      }
    }).el();
  },
  downloadPDFButton: function() {
    var model = this.model;
    var document = this.model.document();
    return new Button({
      size: "big",
      text: localization.authorview.downloadPdf,
      onClick: function() {
        window.open(
          document.mainfile().downloadLinkForMainFile(document.title()),
          '_blank' // <- This is what makes it open in a new window.
        );
      }
    }).el();
  },
  prolongButton : function() {
    var model = this.model;
    var document = this.model.document();
    return new Button({
      type: "action",
      size: "big",
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
      type: "cancel",
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
          content: $('<p>' + modalcontent + '</p>'),
          width: 533,
          acceptText: localization.process.cancelbuttontext,
          rejectText: localization.cancel,
          acceptType: "action",
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
      type: "action",
      size: "big",
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
    var sig = document.signatoriesThatCanSignNowOnPad()[0];
    if (sig == undefined) return; // Should never happend
    return new Button({
      type: "action",
      size: "big",
      text: localization.authorview.goToSignView,
      onClick: function() {
          //Refactor this when we will get confirmation that this is what viktor needs.
          mixpanel.track('Give for pad signing to some pad signatory - opening modal');
          self.signatory = document.signatoriesThatCanSignNow()[0];
          var modalContent = function() {
            var div = $("<div style='height:32px;'/>");
            var label = $('<label style="float: left; padding-right: 10px; line-height: 32px;"></label>').text(localization.pad.giveForSigningThisDevice + " " );
            div.append(label);
            if (document.signatoriesThatCanSignNowOnPad().length > 1)
            {
              var options = [];
              _.each(document.signatoriesThatCanSignNowOnPad(),function(sig) {
                if (sig != model.padNextSignatory())
                options.push({
                  name: (sig.smartname().trim() !== '' ? sig.smartname() : sig.nameInDocument()),
                  onSelect : function() {model.setPadNextSignatory(sig); return true;}
                });
              });
              var $select = $("<span>");
              React.render(React.createElement(Select, {
                name : (model.padNextSignatory().smartname() != "" ? model.padNextSignatory().smartname() : model.padNextSignatory().nameInDocument()),
                className: "float-left",
                options : options
              }), $select[0]);
              div.append($select);
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
                LocalStorage.set("backlink","target","document");
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

    $(this.el).append(container);

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

    buttonbox.append(this.downloadPDFButton());
    container.append(buttonbox);

    return this;
  }

});

var AuthorViewTitleBox = exports.AuthorViewTitleBox = function(args) {
          var model = new AuthorViewTitleBoxModel(args);
          var view =  new AuthorViewTitleBoxView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.destroy = function() { view.destroy();};

};

