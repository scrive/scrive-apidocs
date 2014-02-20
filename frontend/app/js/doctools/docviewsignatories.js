define(['React','authorview/authorviewautomaticreminders', 'Backbone', 'legacy_code'], function(React,Reminders) {

var DocumentViewSignatoriesModel = Backbone.Model.extend({
  defaults : function() { return {
     signatoriesViews : [],
     currentSignview  : undefined,
     onAction : function() {},
     forSigning : false,
     textstyle : {}
    };
  },
  initialize: function (args) {
    var self = this;
    var signatoriesViews = this.get("signatoriesViews");
    _.each(this.signatories(), function(sig) {
      signatoriesViews.push(new DocumentViewSignatory({signatory: sig, authorviewsignatories : self, onAction: args.onAction, forSigning : self.forSigning(), textstyle: self.textstyle() }));
    });
    this.set({currentSignview : this.get('signatoriesViews')[0]}, {silent : true});
  },
  forSigning : function() {
    return this.get("forSigning");
  },
  textstyle: function() {
    return this.get("textstyle");
  },
  signatories: function() {
        var signatories = this.document().signatories();
        var current = _.find  (signatories, function(s) { return  s.current(); });
        var others  = _.filter(signatories, function(s) { return !s.current(); });
        var sigs = [current].concat(others);
        if (this.forSigning())
          return _.filter(sigs, function(s) { return s.signs(); });
        else
          return sigs;

  },
  onAction : function() {
    return this.get("onAction");
  },
  currentIndex : function() {
    for(var i = 0;i < this.get("signatoriesViews").length ; i++ )
      if (this.get("signatoriesViews")[i] == this.get("currentSignview")) return i;
  },
  setCurrentIndex : function(i) {
      if (this.get("signatoriesViews")[i] != undefined)
        this.setCurrentSignview(this.get("signatoriesViews")[i]);
  },
  hasAutomaticReminder : function() {
      return !this.forSigning()
             && this.document().pending()
             && (this.document().timeouttime().diffDays() > 0 || this.document().autoremindtime() != undefined)
             && _.any(this.document().signatories(), function(s) { return s.signs() && !s.hasSigned() && !s.padDelivery(); })
             && this.document().currentViewerIsAuthor();
  },
  document :function() {
     return this.get("document");
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
    this.clear();
  }
});

var DocumentViewSignatoriesView = Backbone.View.extend({
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

          var name       = $("<div class='name' />").text(sigview.nameOrEmailOrMobile()).css(self.model.textstyle());
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
      box.addClass('section').addClass('signatories').css(view.model.textstyle());

      var box1 = $("<div style='width:273px;display:inline-block;'/>");
      var box2 = $("<div style='width:303px;margin-right:15px;margin-left:15px;display:inline-block;;position:relative;'/>");
      var box3 = $("<div style='width:273px;display:inline-block;'/>");
      box.append(box1).append(box2).append(box3);


      var header = $("<h2 style='width: 260px;float:none;padding-left:0px;' />");
      header.css(view.model.textstyle());
      if (view.model.forSigning())
        header.text(localization.docsignview.signatoriesTitle);
      else
        header.text(localization.authorview.signatoriesTitle);
      box1.append(header)

      var s1box = $("<div class='column' style='width:271px;'/>");
      var s2box = $("<div class='column' style='width:271px;'/>");
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
          var model = this.model;
          var rm = $("<div class='column auto-reminder' style='position:absolute;left:-293px;bottom:0px;width:275px;'/>")
          React.renderComponent(Reminders.AuthorViewAutomaticReminders({document : model.document(), onAction : model.onAction()}),rm[0]);
          box2.append(rm);
      }


      return this;
  }

});

window.DocumentViewSignatories = function(args) {
          var model = new DocumentViewSignatoriesModel(args);
          var view =  new DocumentViewSignatoriesView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.currentIndex = function() { return model.currentIndex() };
          this.setCurrentIndex = function(i) { model.setCurrentIndex(i);};
          this.destroy = function() {return view.destroy();};

};

});
