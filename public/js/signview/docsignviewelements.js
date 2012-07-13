/* Signatory view - subviews  */


(function(window) {

window.DocumentSignInstructionsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  isSigning: function() {
    var signatory = this.model.document.currentSignatory();
    return this.model.document.signingInProcess() && signatory.signs() && !signatory.hasSigned();
  },
  isReviewing: function() {
    var signatory = this.model.document.currentSignatory();
    return (this.model.document.signingInProcess() || this.model.document.closed()) && !signatory.signs();
  },
  isSignedNotClosed: function() {
    var signatory = this.model.document.currentSignatory();
    return this.model.document.signingInProcess() && signatory.hasSigned() && !this.model.document.closed();
  },
  isSignedAndClosed: function() {
    var signatory = this.model.document.currentSignatory();
    return signatory.hasSigned() && this.model.document.closed();
  },
  isUnavailableForSign: function() {
    return !this.model.document.signingInProcess() && !this.model.document.closed();
  },
  text: function() {
    if (this.isSigning()) {
      return localization.docsignview.followArrowToSign;
    } else if (this.isReviewing()) {
      return localization.docsignview.followArrowToReview;
    } else if (this.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosed;
    } else if (this.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosed;
    } else if (this.isUnavailableForSign()) {
      return localization.docsignview.unavailableForSign;
    } else {
      console.error("Unsure what state we're in");
      return localization.docsignview.unavailableForSign;
    }
  },
  subtext: function() {
    if (this.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosedSubText;
    } else if (this.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosedSubText;
    } else {
      return "";
    }
  },
  createMenuElems: function() {
    if (this.model.document.padAuthorization()) return $("<div>");  
    return $(new DocumentActionMenuView({
      model: this.model.document,
      el: $("<div class='menuwrapper'/>")
    }).el);
  },
  render: function() {
    $(this.el).empty();

    if(this.model.justSaved())
      return this;

    var container = $("<div class='instructions' />");
    container.append($("<div class='headline' />").text(this.text()));
    container.append($("<div class='subheadline' />").text(this.subtext()));
    if (this.model.document.padAuthorization() && this.isSignedNotClosed() && BrowserInfo.isPadDevice())
    {    var padGiveToNextSignatoryModel = new PadGiveToNextSignatoryModel({document : this.model.document});
         container.append(new PadGiveToNextSignatoryView({model : padGiveToNextSignatoryModel}).el);
    }
    var smallerbit = $("<div />");
    var timeout = this.model.document.timeouttime();
    if (timeout != undefined && this.model.document.signingInProcess()) {
      smallerbit.append($("<div class='duedate' />").text(localization.docsignview.dueDate + " " + timeout.getFullYear() + "-" + timeout.getMonth() + "-" + timeout.getDate()));
    }
    smallerbit.append(this.createMenuElems());
    container.append($("<div class='subheadline' />").append(smallerbit));

    $(this.el).append(container);

    return this;
  }
});

window.DocumentSignSignatoryBox = Backbone.Model.extend({
    defaults: {
        index: 0
    },
    initialize: function(args) {
        var me = this;
        this.document().bind('change', function(){ me.trigger('change:document'); });
    },
    currentSignatoryIndex: function() {
        return this.get('index');
    },
    setSignatoryIndex: function(i) {
        this.set({index:i});
        return this;
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
        return this.signatories()[this.currentSignatoryIndex()];
    }
});

window.DocumentSignSignatoriesView = Backbone.View.extend({
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
  siglist: function(signatories) {
      var sigbox = this.model;
      var list = $("<div class='list spacing' />");
      _.each(signatories, function(signatory, index) {
          var sigdiv     = $("<div class='sig' />");
          if(index === 0)
              sigdiv.addClass('first');
          var name       = $("<div class='name' />").text(signatory.name);
          var line       = $("<div class='line' />");
          var middle1    = $("<div class='middle' />");
          var middle2    = $("<div class='middle' />");
          var middle3    = $("<div class='middle' />");
          var statusicon = $("<div class='icon status' />").addClass(signatory.statusicon);
          var status     = $("<span class='statustext' />").addClass(signatory.statusicon).text(signatory.status);
          var details    = $('<a class="details" href="#" />').text(localization.docsignview.showDetails);

          middle1.append(statusicon);
          middle2.append(status);
          middle3.append(details);
          line.append(middle1).append(middle2).append(middle3);
          details.click(function() {
              sigbox.setSignatoryIndex(index);
              return false;
          });

          sigdiv.append(name).append(line);
          list.append(sigdiv);
      });
      return list;
  },
  statusbox: function(signatory) {
      var statusbox  = $('<div  class="statusbox" />');
      var space = $('<div class="spacing butt" />');
      var statusicon = $("<span class='icon status' />").addClass(signatory.status());
      var status     = $("<span class='status statustext' />").text(this.signatorySummary(signatory)).addClass(signatory.status());
      space.append(statusicon).append(status).addClass(signatory.status());
      statusbox.append(space);
      return statusbox;
  },
  sigbox: function(signatory) {
      var box     = $('<div class="sigbox" />');

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

      box.append(this.statusbox(signatory));

      return box;
  },
  render: function() {
      var view = this;
      var $el = $(this.el);
      $el.empty();

      $el.append($("<h2 />").text(localization.docsignview.signatoriesTitle));
      var box1 = $('<div class="column spacing" />');
      var box2 = $('<div class="column spacing" />');

      $el.append(box1).append(box2);

      var sigbox = view.model;

      var signatories = sigbox.signatories();

      if(signatories.length > 2) {
          var signatories_min = _.map(signatories, function(s) { return {name:s.nameOrEmail(),statusicon:s.status(),status:view.signatorySummary(s)} });
          box1.append(view.siglist(signatories_min));
          box2.append(view.sigbox(sigbox.currentSignatory()));
      } else if (signatories.length === 2) {
          box1.append(view.sigbox(signatories[0]));
          box2.append(view.sigbox(signatories[1]));
      } else if (signatories.length === 1) {
          box1.css("border-color","#ffffff");
          box2.append(view.sigbox(signatories[0]));
      }
      return this;
  }
});

})(window);
