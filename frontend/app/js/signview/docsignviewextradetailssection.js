
define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor) {

window.DocumentExtraDetailsModal = Backbone.View.extend({
  initialize: function(args) {
    this.arrow = args.arrow;
    this.margin = args.margin;
  },


  acceptButton: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var signatory = this.model.document().currentSignatory();

    return new Button({
      color: "green",
      cssClass : "accept-button",
      text: localization.next,
      onClick: function() {
        if (!DocumentExtraDetails.detailsMissing(signatory)) {
          new DocumentSignConfirmation({
            model: self.model,
            fast: true,
            signaturesPlaced: true,
            margin: self.confirmation.absoluteTop() + "px auto 0"
          });

          self.confirmation.clear();
        }  else {
          new FlashMessage({content: localization.docsignview.additionalFieldsMissingInModal, color: "red"});
        }
      }
    });
  },

  emailInput: function() {
    var self = this;
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
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForEmail(signatory));

      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForEmail(signatory));
    return iti.el();
  },
  nameInput: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.fstnameField();
    var iti =  new InfoTextInput({
      infotext: localization.personalName,
      cssClass: 'obligatory-input',
      value: field.value(),
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForName(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForName(signatory));
    return iti.el();
  },
  ssnInput: function() {
    var self = this;
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
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForSSN(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForSSN(signatory));
    return iti.el();
  },
  phoneInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.mobileField();
    var valid = !DocumentExtraDetails.askForPhone(signatory);
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
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForPhone(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForPhone(signatory));
    return iti.el();
  },

  content: function() {
    var signatory = this.model.document().currentSignatory();

    var body = $("<div class='standard-input-table'>");
    body.append($("<p />").text(localization.docsignview.filladitionfieldsdescription));
    var table = $("<table/>");

    if (DocumentExtraDetails.askForName(signatory)) {
      var tr = $("<tr/>").append($("<td/>").text(localization.personalName));
      tr.append($("<td/>").append(this.nameInput(signatory)));
      table.append(tr);
    }

    if (DocumentExtraDetails.askForEmail(signatory)) {
      var tr = $("<tr/>").append($("<td/>").text(localization.email));
      tr.append($("<td/>").append(this.emailInput(signatory)));
      table.append(tr);
    }

    if (DocumentExtraDetails.askForSSN(signatory)) {
      var tr = $("<tr/>").append($("<td/>").text(localization.personalNumber));
      tr.append($("<td/>").append(this.ssnInput(signatory)));
      table.append(tr);
    }
    if (DocumentExtraDetails.askForPhone(signatory)) {
      var tr = $("<tr/>").append($("<td/>").text(localization.phone));
      tr.append($("<td/>").append(this.phoneInput(signatory)));
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
      cssClass: 'grey extradetails-modal',
      title: localization.docsignview.filladitionfields,
      acceptButton: this.acceptButton().el(),
      width: BrowserInfo.isSmallScreen() ? 825 : 535,
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

/* Extra details section for a signatory.
 * Usage:
 *
 *   $('body').append(new DocumentSignSignSection({
 *     model : currentSignatory
 *     arrow: function() { this.get("arrow"); },
 *     signview: this
 *   }).el);
 */
window.DocumentSignExtraDetailsSection = Backbone.View.extend({
  initialize : function(args){
    this.arrow = args.arrow;
    this.signview = args.signview;
    this.render();
  },

  emailInput: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var focused = false;
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
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForEmail(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForEmail(signatory));

    field.bind("change", function() {
      iti.setValue(field.value());
      iti.el().toggleClass("valid",!DocumentExtraDetails.askForEmail(signatory));
    });

    return iti.el();
  },
  nameInput: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var focused = false;
    var field = signatory.fstnameField();
    var iti = new InfoTextInput({
      infotext: localization.personalName,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForName(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForName(signatory));


    field.bind("change", function() {
      iti.setValue(field.value());
      iti.el().toggleClass("valid",!DocumentExtraDetails.askForName(signatory));
    });

    return  iti.el();
  },
  ssnInput: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.personalnumberField();
    var focused = false;
    var iti = new InfoTextInput({
      infotext: localization.personalNumber,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForSSN(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForSSN(signatory));

    field.bind("change", function() {
      iti.setValue(field.value());
      iti.el().toggleClass("valid",!DocumentExtraDetails.askForSSN(signatory));
    });

    return iti.el();
  },
  phoneInput: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.mobileField();
    var iti = new InfoTextInput({
      infotext: localization.phonePlaceholder,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        iti.el().addClass("active");
      },
      onBlur: function() {
        iti.el().removeClass("active");
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");
          iti.el().toggleClass("valid",!DocumentExtraDetails.askForPhone(signatory));
      }
    });
    iti.el().toggleClass("valid",!DocumentExtraDetails.askForPhone(signatory));


    field.bind("change", function() {
      iti.setValue(field.value());
      iti.el().toggleClass("valid",!DocumentExtraDetails.askForPhone(signatory));
    });

    return iti.el();

  },
  render: function() {
      var signatory = this.model;

      var box = $(this.el).addClass('section').addClass('spacing').addClass('extradetails');
      var header = $("<h2 class='title'/>").text(localization.docsignview.filladitionfields);
      var description = $("<div class='column spacing descriptionbox'/>").text(localization.docsignview.filladitionfieldsdescription);
      this.fillBox = $("<div class='column spacing fillbox'/>");

      if (DocumentExtraDetails.askForName(signatory)) {
       this.fillBox.append(this.nameInput(signatory));
      }

      if (DocumentExtraDetails.askForEmail(signatory)) {
       this.fillBox.append(this.emailInput(signatory));
      }

      if (DocumentExtraDetails.askForSSN(signatory)) {
       this.fillBox.append(this.ssnInput(signatory));
      }

      if (DocumentExtraDetails.askForPhone(signatory)) {
       this.fillBox.append(this.phoneInput(signatory));
      }

      box.append(header).append(description).append(this.fillBox).append("<div class='clearfix' />");
  }
});

/*
 * A collection of static methods connected to the extra details.
 */
window.DocumentExtraDetails = Backbone.Model.extend({},
{
  detailsMissing: function(signatory) {
    return DocumentExtraDetails.askForName(signatory)       ||
           DocumentExtraDetails.askForEmail(signatory)      ||
           DocumentExtraDetails.askForSSN(signatory)        ||
           DocumentExtraDetails.askForPhone(signatory);
  },
  askForEmail: function(signatory) {
    var field = signatory.emailField();
    return field != undefined && !new EmailValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
  },
  askForName: function(signatory) {
    var field1 = signatory.fstnameField();
    var field2 = signatory.sndnameField();
    return (field1 != undefined && (field1.value() == "" || field1.value() == undefined) && (!field1.hasPlacements() || !field1.obligatory()) &&
            field2 != undefined && (field2.value() == "" || field2.value() == undefined) && (!field2.hasPlacements() || !field2.obligatory()));

  },
  askForSSN: function(signatory) {
    var field = signatory.personalnumberField();
    return field != undefined && (field.value() == "" || field.value() == undefined) && (!field.hasPlacements()) && field.obligatory();
  },
  askForPhone: function(signatory) {
    var field = signatory.mobileField();
    return field != undefined && !new PhoneValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
  }
});


});
