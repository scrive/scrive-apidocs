define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor) {

window.DocumentExtraDetailsModal = Backbone.View.extend({
  initialize: function(args) {
    this.arrow = args.arrow;
    this.margin = args.margin;
  },


  acceptButton: function() {
    var self = this;
    var signview = this.model;
    var signatory = this.model.document().currentSignatory();

    return new Button({
      type: "action",
      cssClass: "accept-button",
      text: localization.next,
      onClick: function() {
        if (!signview.hasExtraInputs()) {
          new DocumentSignConfirmation({
            model: self.model,
            fast: true,
            signaturesPlaced: true,
            margin: self.confirmation.absoluteTop() + "px auto 0"
          });

          self.confirmation.clear();
        }  else {
          new FlashMessage({content: localization.docsignview.additionalFieldsMissingInModal, type: 'error'});
        }
      }
    });
  },

  emailInput: function() {
    var self = this;
    var signview = this.model;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.emailField();
    var iti = new InfoTextInput({
      infotext: localization.email,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange: function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!signview.askForEmail());

      }
    });
    iti.el().toggleClass("valid",!signview.askForEmail());
    return iti.el();
  },
  nameInput: function() {
    var self = this;
    var signview = this.model;
    var signatory = this.model.document().currentSignatory();
    var fstnameField = signatory.fstnameField();
    var sndnameField = signatory.sndnameField();
    var iti =  new InfoTextInput({
      infotext: localization.personalName,
      cssClass: 'obligatory-input',
      value: signatory.name(),
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange: function(value) {
        var str = value.trim();
        var i = str.indexOf(' ');
        var f, s;
        if(i >= 0) {
          f = str.slice(0,i).trim();
          s = str.slice(i+1).trim();
        } else {
          f = str.trim();
          s = '';
        }
        if (sndnameField != undefined) {
          fstnameField.setValue(f);
          sndnameField.setValue(s);
        } else {
          fstnameField.setValue(str);
        }
        signatory.trigger("change");
        iti.el().toggleClass("valid", !signview.askForName());
      }
    });
    iti.el().toggleClass("valid",!signview.askForName());
    return iti.el();
  },
  ssnInput: function() {
    var self = this;
    var signview = this.model;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.personalnumberField();
    var iti = new InfoTextInput({
      infotext: localization.personalNumber,
      cssClass: 'obligatory-input',
      value: field.value(),
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange: function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!signview.askForSSN());
      }
    });
    iti.el().toggleClass("valid",!signview.askForSSN());
    return iti.el();
  },
  phoneInput: function() {
    var self = this;
    var branding = this.branding;
    var signview = this.model;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.mobileField();
    var focused = false;
    var iti = new InfoTextInput({
      infotext: localization.phonePlaceholder,
      cssClass: 'obligatory-input',
      value: field.value(),
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange: function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!signview.askForPhone());
      }
    });
    iti.el().toggleClass("valid",!signview.askForPhone());
    return iti.el();
  },

  content: function() {
    var signview = this.model;
    var signatory = this.model.document().currentSignatory();

    var body = $("<div class='standard-input-table'>");
    var table = $("<table/>");

    if(signview.askForName()) {
      var tr = $("<tr/>").append($("<td/>").text(localization.personalName));
      tr.append($("<td/>").append(this.nameInput()));
      table.append(tr);
    }
    if(signview.askForEmail()) {
      var tr = $("<tr/>").append($("<td/>").text(localization.email));
      tr.append($("<td/>").append(this.emailInput()));
      table.append(tr);
    }
    if(signview.askForSSN()) {
      var tr = $("<tr/>").append($("<td/>").text(localization.personalNumber));
      tr.append($("<td/>").append(this.ssnInput()));
      table.append(tr);
    }
    if(signview.askForPhone()) {
      var tr = $("<tr/>").append($("<td/>").text(localization.phone));
      tr.append($("<td/>").append(this.phoneInput()));
      table.append(tr);
    }

    body.append(table);

    return body;
  },

  popup: function() {
    var document = this.model.document();
    var signviewbranding = this.model.signviewbranding();
    var signatory = document.currentSignatory();
    var arrow = this.arrow;
    var self = this;

    self.confirmation = new Confirmation({
      cssClass: 'extradetails-modal',
      title: localization.docsignview.filladitionfields,
      acceptButton: this.acceptButton().el(),
      width: BrowserInfo.isSmallScreen() ? 825: 535,
      rejectText: localization.cancel,
      margin: this.margin,
      fast: true,
      onReject: function() {
        if (arrow) { arrow.enable() }
      },
      content: this.content()
    });

    if (arrow) {
      arrow.disable();
    }

  }
});

});
