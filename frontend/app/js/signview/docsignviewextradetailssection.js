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

/* Extra details section for a signatory.
 * Usage:
 *
 *   $('body').append(new DocumentSignSignSection({
 *     model: currentSignatory
 *     arrow: function() { this.get("arrow"); },
 *     signview: this
 *   }).el);
 */
window.DocumentSignExtraDetailsSection = Backbone.View.extend({
  initialize: function(args){
    this.arrow = args.arrow;
    this.signview = args.signview;
    this.render();
  },
  emailInput: function() {
    var self = this;
    if(self.emailInfoTextInput == undefined) {
      var signview = self.signview;
      var signatory = self.model.document().currentSignatory();
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
        onChange: function(value) {
            field.setValue(value);
            signatory.trigger("change");
            iti.el().toggleClass("valid",!signview.askForEmail());
        }
      });
      iti.el().toggleClass("valid",!signview.askForEmail());

      field.bind("change", function() {
        iti.setValue(field.value());
        iti.el().toggleClass("valid",!signview.askForEmail());
      });

      self.emailInfoTextInput = iti;
    }
    return self.emailInfoTextInput;
  },
  focusOnEmailInput: function() {
    if (this.emailInfoTextInput != undefined)
      this.emailInfoTextInput.focus();
  },
  nameInput: function() {
    var self = this;
    if(self.nameInfoTextInput == undefined) {
      var signview = self.signview;
      var signatory = self.model.document().currentSignatory();
      var focused = false;
      var fstnameField = signatory.fstnameField();
      var sndnameField = signatory.sndnameField();
      var iti = new InfoTextInput({
        infotext: localization.personalName,
        value: signatory.name(),
        cssClass: 'obligatory-input',
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
            fstnameField.setValue(f, {origin: iti}); // arguments for event handler
            sndnameField.setValue(s, {origin: iti}); // arguments for event handler
          } else {
            fstnameField.setValue(str, {origin: iti}); // arguments for event handler
          }
          signatory.trigger("change");
          iti.el().toggleClass("valid", !signview.askForName());
        }
      });

      var onNameFieldChange = function(obj, args) {
        // check both name fields to see if full name should be highlighted
        if (!fstnameField.isValid(true) || (sndnameField != undefined && !sndnameField.isValid(true))) {
          iti.el().removeClass('valid');
        } else {
          iti.el().addClass('valid');
        }
        if (args === undefined || args.origin !== iti) {
          // the check above was needed, to know if the change event originated
          // from directly editing this input (in which case we can skip
          // setting the value), otherwise when the input contains "John S"
          // and we backspace the 'S', the space is auto-removed.
          iti.setValue(signatory.name());
        }
      };

      fstnameField.bind('change', onNameFieldChange);
      if (sndnameField != undefined) {
        sndnameField.bind('change', onNameFieldChange);
      }

      iti.el().toggleClass("valid", !signview.askForName());

      self.nameInfoTextInput = iti;
    }
    return self.nameInfoTextInput;
  },
  focusOnNameInput: function() {
    if (this.nameInfoTextInput != undefined)
      this.nameInfoTextInput.focus();
  },
  ssnInput: function() {
    var self = this;
    if(self.ssnInfoTextInput == undefined) {
      var signview = self.signview;
      var signatory = self.model.document().currentSignatory();
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
        onChange: function(value) {
            field.setValue(value);
            signatory.trigger("change");
            iti.el().toggleClass("valid", !signview.askForSSN());
        }
      });
      iti.el().toggleClass("valid", !signview.askForSSN());

      field.bind("change", function() {
        iti.setValue(field.value());
        iti.el().toggleClass("valid", !signview.askForSSN());
      });

      self.ssnInfoTextInput = iti;
    }
    return self.ssnInfoTextInput;
  },
  focusOnSsnInput: function() {
    if (this.ssnInfoTextInput != undefined)
      this.ssnInfoTextInput.focus();
  },
  phoneInput: function() {
    var self = this;
    if(self.phoneInfoTextInput == undefined) {
      var signview = self.signview;
      var signatory = self.model.document().currentSignatory();
      var field = signatory.mobileField();
      var iti = new InfoTextInput({
        infotext: localization.phonePlaceholder,
        value: field.value(),
        cssClass: 'obligatory-input extradetails-phone',
        onFocus: function() {
          iti.el().addClass("active");
        },
        onBlur: function() {
          iti.el().removeClass("active");
        },
        onChange: function(value) {
            field.setValue(value);
            signatory.trigger("change");
            iti.el().toggleClass("valid", !signview.askForPhone());
        }
      });
      iti.el().toggleClass("valid", !signview.askForPhone());


      field.bind("change", function() {
        iti.setValue(field.value());
        iti.el().toggleClass("valid", !signview.askForPhone());
      });

      self.phoneInfoTextInput = iti;
    }
    return self.phoneInfoTextInput;
  },
  focusOnPhoneInput: function() {
    if (this.phoneInfoTextInput != undefined)
      this.phoneInfoTextInput.focus();
  },
  render: function() {
      var signatory = this.model;
      var signview = this.signview;


      var box = $(this.el).addClass('section').addClass('spacing').addClass('extradetails');
      var header = $("<h2 class='title'/>").text(localization.docsignview.filladitionfieldslabel);
      this.fillBox = $("<div class='column spacing fillbox'/>");

      if (signview.askForName()) {
       this.fillBox.append(this.nameInput().el());
      }

      if (signview.askForEmail()) {
       this.fillBox.append(this.emailInput().el());
      }

      if (signview.askForSSN()) {
       this.fillBox.append(this.ssnInput().el());
      }

      if (signview.askForPhone()) {
       this.fillBox.append(this.phoneInput().el());
      }

      box.append(header).append(this.fillBox).append("<div class='clearfix' />");
  }
});

});
