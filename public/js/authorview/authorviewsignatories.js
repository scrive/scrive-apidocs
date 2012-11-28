(function(window){

var AuthorViewSignatoriesModel = Backbone.Model.extend({
  defaults : {
     signatoriesViews : [],
     currentSignview  : undefined
  },
  initialize: function (args) {
    var self = this;
    var signatoriesViews = this.get("signatoriesViews");
    _.each(this.document().signatories(), function(sig) {
      signatoriesViews.push(new AuthorViewSignatory({signatory: sig, authorviewsignatories : self }));
    });
    this.set({currentSignview : this.get('signatoriesViews')[0]}, {silent : true});
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
  hasDummy : function() {
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
  }
});

var AuthorViewSignatoriesView = Backbone.View.extend({
  initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
        this.model.bind('change', this.render);
  },
  list : function() {
      var model = this.model;
      var list = $("<div class='list spacing'>");
      _.each(this.model.signatoriesViews(), function(sigview, index) {
          var sigdiv     = $("<div class='sig' />");
          if(index === 0)
              sigdiv.addClass('first');
          var name       = $("<div class='name' />").text(sigview.nameOrEmail());
          var line       = $("<div class='line' />");
          var middle1    = $("<div class='middle' />");
          var middle2    = $("<div class='middle' />");
          var middle3    = $("<div class='middle float-right' />");
          var statusicon = $("<div class='icon status' />").addClass(sigview.status());
          var status     = $("<span class='statustext' />").addClass(sigview.status()).text(sigview.signatorySummary());
          var details    = $('<a class="details" href="#" />').text(localization.docsignview.showDetails);
          middle1.append(statusicon);
          middle2.append(status);
          middle3.append(details);
          line.append(middle1).append(middle2).append(middle3);
          details.click(function() {
              model.setCurrentSignview(sigview);
              return false;
          });
          sigdiv.append(name).append(line);
          list.append(sigdiv);
      });
    return list;
  },
  render: function() {
      var view = this;
      var box = $(this.el);
      box.children().detach();
      box.addClass('section').addClass('signatories').addClass('spacing');

      box.append($("<h2 />").text(localization.authorview.signatoriesTitle));

      var table = $("<table class='signatories-box'/>");
      var tbody = $("<tbody/>");
      var tr = $("<tr/>")
      var td1 = $("<td class='signatory-box'/>");
      var tdseparator = $("<td class='signatory-box-separator'/>");
      var td2 = $("<td class='signatory-box'/>")
      var box1 = $('<div class="column spacing" />');
      var box2 = $('<div class="column spacing" />');
      table.append(tbody.append(tr.append(td1.append(box1)).append(tdseparator).append(td2.append(box2))));
      box.append(table);

      if (this.model.hasDummy()) {
         box1.css("border-color","#ffffff");
         box2.append(this.signatoryView().el());
      }   
      else if (this.model.hasList()) {
         box1.append(this.list());
         box2.append(this.model.signatoryView().el());
         
      } else {
         box1.append(this.model.signatoryView(0).el());
         box2.append(this.model.signatoryView(1).el());
      }

      return this;
  }

});

window.AuthorViewSignatories = function(args) {
          var model = new AuthorViewSignatoriesModel(args);
          var view =  new AuthorViewSignatoriesView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);