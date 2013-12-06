(function(window){

var AuthorViewSignatoriesModel = Backbone.Model.extend({
  defaults : function() { return {
     signatoriesViews : [],
     currentSignview  : undefined
    };
  },
  initialize: function (args) {
    var self = this;
    var signatoriesViews = this.get("signatoriesViews");
    _.each(this.signatories(), function(sig) {
      signatoriesViews.push(new AuthorViewSignatory({signatory: sig, authorviewsignatories : self }));
    });
    this.set({currentSignview : this.get('signatoriesViews')[0]}, {silent : true});
  },
  signatories: function() {
    return this.document().signatories();
  },
  currentIndex : function() {
    for(var i = 0;i < this.get("signatoriesViews").length ; i++ )
      if (this.get("signatoriesViews")[i] == this.get("currentSignview")) return i;
  },
  setCurrentIndex : function(i) {
      if (this.get("signatoriesViews")[i] != undefined)
        this.setCurrentSignview(this.get("signatoriesViews")[i]);
  },
  automaticreminder :  function() {
      if (this.get("automaticreminder") == undefined)
        this.set({"automaticreminder" : new AuthorViewAutomaticReminders({authorview : this.authorview()})}, {silent : true});
      return this.get("automaticreminder");
  },
  hasAutomaticReminder : function() {
      return this.document().pending() && (this.document().timeouttime().diffDays() > 0 || this.document().autoremindtime() != undefined);
  },
  authorview :function() {
     return this.get("authorview");
  },
  document :function() {
     return this.authorview().document();
  },
  hasList : function() {
     return this.signatoriesViews().length > 2;
  },
  isSingleSignatory : function() {
     return this.signatoriesViews().length ==1;
  },
  signatoriesViews : function() {
     return this.get("signatoriesViews");
  },
  signatoryView : function(i)
  {
    if (i == undefined)  return this.get("currentSignview");
    return this.signatoriesViews()[i];
  },
  setCurrentSignview : function(sv) {
    this.set({currentSignview : sv});
  },
  destroy : function() {
    _.each(this.signatoriesViews(), function(s) {s.destroy();});
    if (this.hasAutomaticReminder())
      this.automaticreminder().destroy();
    this.clear();
  }
});

var AuthorViewSignatoriesView = Backbone.View.extend({
  initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
        this.listenTo(this.model,'change', this.render);
  },
  destroy : function() {
    this.stopListening();
    this.model.off();
    this.model.destroy();
    $(this.el).remove();
  },
  list : function() {
      var self = this;
      var model = this.model;
      this.listDiv = $("<div class='list spacing'>");
      _.each(this.model.signatoriesViews(), function(sigview, index) {
          var sigdiv     = $("<div class='sig' />");
          if(index === 0)
              sigdiv.addClass('first');
          var name       = $("<div class='name' />").text(sigview.nameOrEmailOrMobile());
          var line       = $("<div class='line' />");
          var middle1    = $("<div class='middle'/>");
          var middle2    = $("<div class='middle' style='min-width: 150px;'/>");
          var middle3    = $("<div class='middle details' style='white-space: nowrap;'/>");
          var statusicon = $("<div class='icon status'/>").addClass(sigview.status());
          var status     = $("<div class='statustext' />").addClass(sigview.status()).html(sigview.signatoryViewerySummary());
          var details    = $('<a class="clickable" href="#" />').text(localization.docsignview.showDetails);
          middle1.append(statusicon);
          middle2.append(status);
          middle3.append(details);
          line.append(middle1).append(middle2).append(middle3);
          details.click(function() {
              model.setCurrentSignview(sigview);
              return false;
          });
          sigdiv.append(name).append(line);
          self.listDiv.append(sigdiv);
      });
    return this.listDiv;
  },
  render: function() {
      var view = this;
      var box = $(this.el);
      if (this.listDiv!= undefined) this.listDiv.remove();
      box.children().detach();
      box.addClass('section').addClass('signatories').addClass('spacing');

      var header = $("<h2 style='width: 100px;' />");
      box.append(header.text(localization.authorview.signatoriesTitle));

      var container = $("<div class='signatories-box' style='float: right; width: 620px' />");

      var leftcontainer = $("<div class='float-left' style='width:300px;margin-right:20px'>");
      var box1 = $("<div class='signatory-box column spacing' style='width:260px' />");
      var rightcontainer = $("<div class='float-right' style='width:300px';>");
      var box2 = $("<div class='signatory-box column spacing float-right' style='width:260px'/>");
      box.append(container.append(leftcontainer.append(box1)).append(rightcontainer.append(box2)));

      if (this.model.isSingleSignatory()) {
         box1.removeClass("signatory-box");
         box2.append(this.model.signatoryView().el());
      }
      else if (this.model.hasList()) {
         box1.append(this.list());
         box2.append(this.model.signatoryView().el());

      } else {
         box1.append(this.model.signatoryView(0).el());
         box2.append(this.model.signatoryView(1).el());
      }

      if (this.model.hasAutomaticReminder()) {
         var box3 = $("<div class='signatory-box column spacing float-right' style='margin-top:20px;width:260px'/>").append(this.model.automaticreminder().el());
         rightcontainer.append(box3);
      }
      return this;
  }

});

window.AuthorViewSignatories = function(args) {
          var model = new AuthorViewSignatoriesModel(args);
          var view =  new AuthorViewSignatoriesView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.currentIndex = function() { return model.currentIndex() };
          this.setCurrentIndex = function(i) { model.setCurrentIndex(i);};
          this.destroy = function() {return view.destroy();};

};


})(window);
