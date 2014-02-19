define(['Backbone', 'legacy_code'], function() {

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
      return this.document().pending()
             && (this.document().timeouttime().diffDays() > 0 || this.document().autoremindtime() != undefined)
             && _.any(this.document().signatories(), function(s) { return s.signs() && !s.hasSigned() && !s.padDelivery(); })
             && this.document().currentViewerIsAuthor();
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
          if (index === self.model.signatoriesViews().length - 1)
              sigdiv.addClass('last');
          if (index == self.model.currentIndex())
              sigdiv.addClass('active');
          if (index == self.model.currentIndex())
              sigdiv.append("<div class='arrow'/>");

          var name       = $("<div class='name' />").text(sigview.nameOrEmailOrMobile());
          var line       = $("<div class='line' />");
          var middle1    = $("<div class='middle'/>");
          var middle2    = $("<div class='middle' style='min-width: 150px;'/>");
          var middle3    = $("<div class='middle details' style='white-space: nowrap;'/>");
          var statusicon = $("<div class='icon status'/>").addClass(sigview.status());
          var status     = $("<div class='statustext' />").addClass(sigview.status()).html(sigview.signatoryViewerySummary());
          middle1.append(statusicon);
          middle2.append(status);
          line.append(middle1).append(middle2).append(middle3);
          sigdiv.click(function() {
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

      var box1 = $("<div style='width:275px;margin-left:36px;display:inline-block;'/>");
      var box2 = $("<div style='width:275px;margin-right:18px;margin-left:18px;display:inline-block;;position:relative;'/>");
      var box3 = $("<div style='width:275px;margin-right:36px;display:inline-block;'/>");
      box.append(box1).append(box2).append(box3);


      var header = $("<h2 style='width: 100px;float:none;padding-left:0px;' />").text(localization.authorview.signatoriesTitle);
      box1.append(header)

      var s1box = $("<div class='column' />");
      var s2box = $("<div class='column' />");
      box2.append(s1box);
      box3.append(s2box);

      if (this.model.isSingleSignatory()) {
         s1box.append(this.model.signatoryView().el()).addClass("grey-box");
      }
      else if (this.model.hasList()) {
         s1box.append(this.model.signatoryView().el()).addClass("grey-box");
         s2box.append(this.list()).addClass("grey-box").css("padding","0px");

      } else {
         s1box.append(this.model.signatoryView(0).el()).addClass("grey-box");
         s2box.append(this.model.signatoryView(1).el()).addClass("grey-box");
      }

      if (this.model.hasAutomaticReminder()) {
         var rm = $("<div class='column auto-reminder' style='position:absolute;left:-293px;bottom:0px;'/>").append(this.model.automaticreminder().el()).addClass("grey-box");
         box2.append(rm);
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

});
