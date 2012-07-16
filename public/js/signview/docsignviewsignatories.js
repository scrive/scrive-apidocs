/* Signatory view - subviews  */


(function(window) {

    
window.DocumentSignSignatoriesModel = Backbone.Model.extend({
    defaults: {
        current : 0
    },
    initialize: function(args) {
        var me = this;
        this.setCurrentSignatory(this.signatories()[0]);
        this.document().bind('change', function(){ me.trigger('change:document'); });
    },
    setCurrentSignatory: function(sig) {
        this.set({current:sig});
    },
    document: function() {
        return this.get('document');
    },
    signatories: function() {
        var signatories = this.document().signatories();
        var current = _.find  (signatories, function(s) { return  s.current(); });
        var others  = _.filter(signatories, function(s) { return !s.current(); });
        return _.filter([current].concat(others), function(s) { return s.signs(); });
    },
    currentSignatory: function() {
        return this.get("current");
    }
});

var DocumentSignSignatoriesListView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
 signatorySummary: function(signatory) {
      var document = signatory.document();
      if (signatory.signdate() != undefined)
        return localization.signatoryMessage.signed;
      else if (signatory.datamismatch() == true ||
               document.timedout() ||
               document.canceled() ||
               document.datamismatch())
          return localization.docsignview.unavailableForSign;
      else if (signatory.rejecteddate() != undefined)
          return localization.signatoryMessage.rejected;
      else if (signatory.status() == 'opened')
          return localization.signatoryMessage.seen;
      else if (signatory.status() == 'sent')
          return localization.signatoryMessage.other;
      else
          return localization.signatoryMessage[signatory.status()];
  },
  render: function() {
      var sigbox = this.model;
      var view = this;
      var list = $(this.el);
      list.empty();
      list.addClass('list').addClass('spacing');
      _.each(this.model.signatories(), function(signatory, index) {
          var sigdiv     = $("<div class='sig' />");
          if(index === 0)
              sigdiv.addClass('first');
          var name       = $("<div class='name' />").text(signatory.nameOrEmail());
          var line       = $("<div class='line' />");
          var middle1    = $("<div class='middle' />");
          var middle2    = $("<div class='middle' />");
          var middle3    = $("<div class='middle' />");
          var statusicon = $("<div class='icon status' />").addClass(signatory.status());
          var status     = $("<span class='statustext' />").addClass(signatory.status()).text(view.signatorySummary(signatory));
          var details    = $('<a class="details" href="#" />').text(localization.docsignview.showDetails);

          middle1.append(statusicon);
          middle2.append(status);
          middle3.append(details);
          line.append(middle1).append(middle2).append(middle3);
          details.click(function() {
              sigbox.setCurrentSignatory(signatory);
              return false;
          });

          sigdiv.append(name).append(line);
          list.append(sigdiv);
      });
      return this;
  }
});

var DocumentSignSignatoryView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  signatorySummary: function() {
      var signatory = this.model;
      var document = signatory.document();
      if (signatory.signdate() != undefined)
        return localization.signatoryMessage.signed;
      else if (signatory.datamismatch() == true ||
               document.timedout() ||
               document.canceled() ||
               document.datamismatch())
          return localization.docsignview.unavailableForSign;
      else if (signatory.rejecteddate() != undefined)
          return localization.signatoryMessage.rejected;
      else if (signatory.status() == 'opened')
          return localization.signatoryMessage.seen;
      else if (signatory.status() == 'sent')
          return localization.signatoryMessage.other;
      else
          return localization.signatoryMessage[signatory.status()];
  },
  statusbox: function() {
      var signatory = this.model;
      var statusbox  = $('<div  class="statusbox" />');
      var space = $('<div class="spacing butt" />');
      var statusicon = $("<span class='icon status' />").addClass(signatory.status());
      var status     = $("<span class='status statustext' />").text(this.signatorySummary()).addClass(signatory.status());
      space.append(statusicon).append(status).addClass(signatory.status());
      statusbox.append(space);
      return statusbox;
  },
  tagname : 'div',
  render: function() {
      var box = $(this.el);
      box.empty();
      box.addClass('sigbox');
      var signatory = this.model;
      var titleinfo = $('<div class="titleinfo spacing" />');
      var name      = $('<div class="name" />').text(signatory.name());
      var company   = $('<div class="company" />').text(signatory.company());
      titleinfo.append(name).append(company);
      box.append(titleinfo);

      var inner   = $('<div class="inner spacing" />');

      var face    = $('<div class="face" />');

      var numspace = $('<div class="spacing numspace" />');
      var orgnum  = $('<div class="orgnum field" />').text(localization.docsignview.companyNumberLabel + ": "
                                                           + (signatory.companynumber().trim() || localization.docsignview.notEntered))
          .attr('title', signatory.companynumber());
      var persnum = $('<div class="persnum field" />').text(localization.docsignview.personalNumberLabel + ": "
                                                            + (signatory.personalnumber().trim() || localization.docsignview.notEntered))
        .attr('title', signatory.personalnumber());
      var contactspace = $('<div class="spacing contactspace" />');
      var email   = $('<div class="email field" />').text(signatory.email()).attr('title', signatory.email());

      numspace.append(orgnum);
      numspace.append(persnum);

      numspace.append(email);

      inner.append(face);

      inner.append(numspace);
      inner.append(contactspace);
      box.append(inner);

      box.append(this.statusbox());
      return this;
  }
});

window.DocumentSignSignatoriesView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  tagname : 'div',
  render: function() {
      var view = this;
      var box = $(this.el);
      box.empty();
      box.addClass('section').addClass('signatories').addClass('spacing');

      box.append($("<h2 />").text(localization.docsignview.signatoriesTitle));
      var box1 = $('<div class="column spacing" />');
      var box2 = $('<div class="column spacing" />');

      box.append(box1).append(box2);

      var sigbox = view.model;

      var signatories = sigbox.signatories();

      if(signatories.length > 2) {
          box1.append(new DocumentSignSignatoriesListView({model : this.model}).el);
          box2.append(new DocumentSignSignatoryView({model : this.model.currentSignatory()}).el);
      } else if (signatories.length === 2) {
          box1.append(new DocumentSignSignatoryView({model : this.model.signatories()[0]}).el);
          box2.append(new DocumentSignSignatoryView({model : this.model.signatories()[1]}).el);
      } else if (signatories.length === 1) {
          box1.css("border-color","#ffffff");
          box2.append(new DocumentSignSignatoryView({model : this.model.signatories()[0]}).el);
      }
      return this;
  }
});

})(window);
