
define(['../../libs/tinycolor-min', 'Backbone', 'legacy_code'], function(tinycolor) {

window.DocumentExtraDetailsModal = Backbone.View.extend({
  initialize: function(args) {
    this.branding = args.branding;
    this.arrow = args.arrow;
    this.margin = args.margin;

    var color = this.branding && this.branding.signviewprimarycolour() ? this.branding.signviewprimarycolour() : '#53b688';
    this.standardBorderColor = tinycolor(color);
    this.highlightedBorderColor = tinycolor(color);
    this.validBorderColor = '#ddd';

    this.highlightedBorderColor.setAlpha(1);
    this.standardBorderColor.setAlpha(0.6);
  },


  acceptButton: function() {
    var self = this;
    var signatory = this.model.document().currentSignatory();
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();

    return new Button({
      color: "green",
      customcolor: branding ? branding.signviewprimarycolour() : undefined,
      textcolor: branding ? branding.signviewprimarytextcolour() : undefined,
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
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var valid = !DocumentExtraDetails.askForEmail(signatory);
    var focused = false;
    var field = signatory.emailField();
    var iti = new InfoTextInput({
      infotext: localization.email,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForEmail(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            // I don't know if I'm in hover or focused mode here.
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);
    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    return el;
  },
  nameInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var valid = !DocumentExtraDetails.askForName(signatory);
    var focused = false;
    var field = signatory.fstnameField();
    var iti =  new InfoTextInput({
      infotext: localization.personalName,
      cssClass: 'obligatory-input',
      value: field.value(),
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForName(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);
    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    return el;
  },
  ssnInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.personalnumberField();
    var valid = !DocumentExtraDetails.askForSSN(signatory);
    var focused = false;
    var iti = new InfoTextInput({
      infotext: localization.personalNumber,
      cssClass: 'obligatory-input',
      value: field.value(),
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForSSN(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);

    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    return el;
  },
  phoneInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.mobileField();
    var valid = !DocumentExtraDetails.askForPhone(signatory);
    var focused = false;
    var iti = new InfoTextInput({
      infotext: localization.phone,
      cssClass: 'obligatory-input',
      value: field.value(),
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForPhone(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);

    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    return el;
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
    this.textstyle = args.textstyle;
    this.arrow = args.arrow;
    this.signview = args.signview;
    this.branding = args.signviewbranding;

    var color = this.branding && this.branding.signviewprimarycolour() ? this.branding.signviewprimarycolour() : '#53b688';
    this.standardBorderColor = tinycolor(color);
    this.highlightedBorderColor = tinycolor(color);
    this.validBorderColor = '#ddd';

    this.highlightedBorderColor.setAlpha(1);
    this.standardBorderColor.setAlpha(0.6);

    this.render();
  },

  emailInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var valid = !DocumentExtraDetails.askForEmail(signatory);
    var focused = false;
    var field = signatory.emailField();
    var iti = new InfoTextInput({
      infotext: localization.email,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForEmail(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);
    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    field.bind("change", function() {
      iti.setValue(field.value());

      if (!DocumentExtraDetails.askForEmail(signatory)) {
        valid = true;
        iti.el().css("border-color", self.validBorderColor);
      } else {
        valid = false;
        iti.el().css("border-color", self.highlightedBorderColor);
      }
    });

    return el;
  },
  nameInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var valid = !DocumentExtraDetails.askForName(signatory);
    var focused = false;
    var field = signatory.fstnameField();
    var iti = new InfoTextInput({
      infotext: localization.personalName,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForName(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);
    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    field.bind("change", function() {
      iti.setValue(field.value());

      if (!DocumentExtraDetails.askForName(signatory)) {
        valid = true;
        iti.el().css("border-color", self.validBorderColor);
      } else {
        valid = false;
        iti.el().css("border-color", self.highlightedBorderColor);
      }
    });

    return el;
  },
  ssnInput: function() {
    var self = this;
    var branding = this.branding;
    var signatory = this.model.document().currentSignatory();
    var field = signatory.personalnumberField();
    var valid = !DocumentExtraDetails.askForSSN(signatory);
    var focused = false;
    var iti = new InfoTextInput({
      infotext: localization.personalNumber,
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForSSN(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);

    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    field.bind("change", function() {
      iti.setValue(field.value());

      if (!DocumentExtraDetails.askForSSN(signatory)) {
        valid = true;
        iti.el().css("border-color", self.validBorderColor);
      } else {
        valid = false;
        iti.el().css("border-color", self.highlightedBorderColor);
      }
    });

    return el;
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
      value: field.value(),
      cssClass: 'obligatory-input',
      onFocus: function() {
        if (valid) return;

        focused = true;
        iti.el().css("border-color", self.highlightedBorderColor);
      },
      onBlur: function() {
        if (valid) return;

        focused = false;
        iti.el().css("border-color", self.standardBorderColor);
      },
      onChange : function(value) {
          field.setValue(value);
          signatory.trigger("change");

          if (!DocumentExtraDetails.askForPhone(signatory)) {
            valid = true;
            iti.el().css("border-color", self.validBorderColor);
          } else {
            valid = false;
            iti.el().css("border-color", self.highlightedBorderColor);
          }
      }
    });
    var el = iti.el();
    iti.el().css("border-color", this.standardBorderColor);

    if (valid) {
      iti.el().css("border-color", this.validBorderColor);

    }

    el.hover(function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.highlightedBorderColor);
    }, function() {
      if (focused || valid) return;
      iti.el().css("border-color", self.standardBorderColor);
    });

    field.bind("change", function() {
      iti.setValue(field.value());

      if (!DocumentExtraDetails.askForPhone(signatory)) {
        valid = true;
        iti.el().css("border-color", self.validBorderColor);
      } else {
        valid = false;
        iti.el().css("border-color", self.highlightedBorderColor);
      }
    });

    return el;
  },
  signatureInput: function() {
    var signatory = this.model.document().currentSignatory();
    var field = signatory.signatures()[0];

    field.bind("change", function() {signatory.trigger("change");});

    return new SignaturePlacementViewForDrawing({
      model: field,
      height: 102,
      width: 260,
      arrow: this.arrow,
      signview: this.signview,
      signviewbranding: this.branding
    }).el;
  },
  render: function() {
      var signatory = this.model;

      var box = $(this.el).addClass('section').addClass('spacing').addClass('extradetails');
      var header = $("<h2 class='title'/>").text(localization.docsignview.filladitionfields);
      header.css(this.textstyle);
      var description = $("<div class='column spacing descriptionbox'/>").text(localization.docsignview.filladitionfieldsdescription);
      description.css(this.textstyle);
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

      if (DocumentExtraDetails.askForSignature(signatory)) {
       this.fillBox.append(this.signatureInput(signatory));
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
           DocumentExtraDetails.askForPhone(signatory)        ||
           DocumentExtraDetails.askForSignature(signatory);
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
  },
  askForSignature : function(signatory) {
    if(signatory.padDelivery() && signatory.hasSignatureField()) {
      return !signatory.anySignatureHasImageOrPlacement();
    }

    return false;
  }
});


});
