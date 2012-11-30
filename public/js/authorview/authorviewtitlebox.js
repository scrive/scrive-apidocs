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
    return this.document().canberestarted();
  },
  restart : function() {
    this.document().restart().send();
  },
  canBeWithdrawn : function() {
    return this.document().canbecanceled();
  },
  cancel : function() {
    this.document().cancel().sendAjax(function() {window.location = window.location;});
  },
  canGoToSignView : function() {
    return this.document().currentViewerIsAuthor() && this.document().currentSignatoryCanSign();
  },
  goToSignView : function() {
    new Submit({method: 'POST', url : '/d/signview/' + this.document().documentid()}).send();
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
      var timeoutText = timeout.getFullYear() + "-" + (timeout.getMonth() < 9 ? "0" + (timeout.getMonth() + 1) : (timeout.getMonth()+1)) + "-" + timeout.getDate();
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
              model.cancel();
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
      text: "Go to Sign View",
      onClick: function() {
         model.goToSignView();
        }
    }).input();
  },
  render: function() {
    var document = this.model.document();
    $(this.el).empty();

    var container = $("<div class='titlebox' />");
    container.append($("<div class='headline' />").text(this.text()));

    if (document.timeouttime() != undefined && document.signingInProcess())
      container.append($("<div class='subheadline duedate' />").text(this.dueDateDescription()));


    $(this.el).append(container);

    if (this.model.hasButtons()) {
      var buttonbox = $("<div class='buttonbox'/>");
      if (this.model.canBeRestarted())
        buttonbox.append(this.restartButton());
      if (this.model.canBeWithdrawn())
        buttonbox.append(this.withdrawnButton());
      if (this.model.canGoToSignView())
        buttonbox.append(this.goToSignViewButton());
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