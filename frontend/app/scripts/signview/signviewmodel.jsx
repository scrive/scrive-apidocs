define(["React", "signview/create_account_section_view", "signview/signatories/docviewsignatories",
        "signview/attachments/signatoryattachmentsview", "signview/instructionsview/instructionsview",
        "signview/attachments/authorattachmentsview", "signview/extradetails/extradetailsview",
        "signview/signsection/signsectionview", "common/retargeting_service",
        "signview/fileview/fileclass", "Backbone", "Underscore", "legacy_code"],
  function (React, CreateAccountSection, DocumentViewSignatories,
            SignatoryAttachmentsView, InstructionsView, AuthorAttachmentsView,
            ExtraDetailsView, SignSectionView, RetargetingService, FileClass,
            Backbone, _) {

    return Backbone.Model.extend({
    defaults: {
      hasChangedPin: false,
      tasks: []
    },

    initialize: function (args) {
      var model = this;
      var document = args.document;
      var triggeredChangeOnReady = false;

      _.bindAll(this, "blockReload");

      document.bind("reset", function () {
        model.trigger("change");
      });

      document.bind("change", function () {
          if (!model.isReady()) {
            model.trigger("change");
          } else if (!triggeredChangeOnReady) { // Once model is ready - we want to trigger change only once
            triggeredChangeOnReady = true;
            model.trigger("change");
          }
        });

      document.bind("file:change placements:change", function () {
          var arrow = model.get("arrow");
          if (arrow !== undefined) {
            arrow.deletePageTasksArrow();
          }
          model.set("arrow", undefined, {silent: true});
          model.trigger("change");
        });
    },

    blockReload: function () {
      var signatory = this.document().currentSignatory();

      var changedAnyFields = _.any(signatory.fields(), function (field) { return field.hasChanged(); });

      var changedAnyAttachments = _.any(signatory.attachments(), function (attachment) {
        return attachment.hasChanged();
      });

      var changedPin = this.hasChangedPin();

      if (changedAnyFields || changedAnyAttachments || changedPin) {
        return localization.signingStartedDontCloseWindow;
      }
    },

    document: function () {
      return this.get("document");
    },

    allowsavesafetycopy: function () {
      return this.get("allowsavesafetycopy");
    },

    isReady: function () {
      return this.document().ready() && this.document().mainfile() != undefined;
    },

    noMainFile: function () {
      return this.document().ready() && this.document().mainfile() == undefined;
    },

    hasAnySection: function () {
      return this.hasMainFileSection()
        || this.hasAuthorAttachmentsSection()
        || this.hasExtraDetailsSection()
        || this.hasSignatoriesAttachmentsSection()
        || this.hasCreateAccountSection()
        || this.hasSignSection();
    },

    hasChangedPin: function () {
      return this.get("hasChangedPin");
    },

    hasRejectOption: function () {
      return this.document().showrejectoption();
    },

    hasMainFileSection: function () {
      return this.document().ready() && this.document().mainfile() != undefined;
    },

    hasSignSection: function () {
      var signatory = this.document().currentSignatory();
      return this.document().currentSignatoryCanSign() && this.hasArrows();
    },

    hasSignatoriesSection: function () {
      return !this.document().closed()
             && !BrowserInfo.isSmallScreen()
             && _.filter(this.document().signatories(), function (sig) {return sig.signs();}).length > 1;
    },

    hasAuthorAttachmentsSection: function () {
      return this.document().authorattachments().length > 0;
    },

    hasExtraDetailsSection: function () {
      if (this.get("hasExtraDetailsSection") !== undefined) {
        return this.get("hasExtraDetailsSection");
      }
      if (!this.document().currentSignatoryCanSign()) {
        return false;
      }
      if (this.hasExtraInputs()) {
        this.set({"hasExtraDetailsSection": true}, {silent: true});
        return true;
      }
      return false;
    },

    hasExtraInputs: function () {
      return this.askForName()
          || this.askForEmail()
          || this.askForSSN()
          || this.askForPhone();
    },

    askForName: function () {
      var signatory = this.document().currentSignatory();
      var field1 = signatory.fstnameField();
      var field2 = signatory.sndnameField();
      var hasField1 = field1 != undefined &&
        (field1.value() == "" || field1.value() == undefined) && (!field1.hasPlacements() || !field1.obligatory());
      var hasField2 = field2 != undefined &&
        (field2.value() == "" || field2.value() == undefined) && (!field2.hasPlacements() || !field2.obligatory());
      return hasField1 && hasField2;
    },

    askForEmail: function () {
      var signatory = this.document().currentSignatory();
      var field = signatory.emailField();
      return field != undefined &&
        !new EmailValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
    },

    askForSSN: function () {
      var signatory = this.document().currentSignatory();
      var field = signatory.personalnumberField();
      if (field != undefined && (!field.hasPlacements()) && field.obligatory()) {
        if (signatory.noBankIDAuthenticationToView()) {
          return !new SSNForNOBankIDValidation().validateData(field.value());
        } else {
          return !new SSNForSEBankIDValidation().validateData(field.value());
        }
      } else {
        return false;
      }
    },

    askForPhone: function () {
      var signatory = this.document().currentSignatory();
      var field = signatory.mobileField();
      return field != undefined
        && !new PhoneValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
    },

    hasSignatoriesAttachmentsSection: function () {
      return this.document().currentSignatory().attachments().length > 0;
    },

    hasArrows: function () {
      return this.document().ready() && this.document().currentSignatoryCanSign();
    },

    hasCreateAccountSection: function () {
      var document = this.document();
      return document.currentSignatory() != undefined
         && !document.currentSignatory().saved()
         && document.currentSignatory().email() // We assume that if this email is set - then it is valid
         && !document.currentSignatory().padDelivery()
         && !document.currentSignatory().apiDelivery()
         && document.currentSignatory().hasSigned()
         && this.allowsavesafetycopy();
    },

    showRetargetingPixel: function () {
      if (this.get("retargeted") == undefined) {
        this.set({"retargeted": true}, {silent: true});
        // At the moment, nothing has to be added to the DOM.
        RetargetingService.addRetargeting();
      }
    },

    sendTrackingData: function () {
      var signatory = this.document().currentSignatory();

      var sps = {};
      sps["Has user?"] = signatory.hasUser();
      sps["First visit"] = !signatory.seendate();
      mixpanel.register(sps);

      var ps = {};
      ps["Full Name"] = signatory.nameOrEmail();
      ps.$email = signatory.email();
      if (signatory.fstname()) {
        ps.$first_name = signatory.fstname();
      }

      if (signatory.sndname()) {
        ps.$last_name = signatory.sndname();
      }

      if (signatory.hasUser()) {
        ps.$username = signatory.email();
      }

      mixpanel.people.set(ps);

      mixpanel.track("View sign view");
    },

    takeFirstScreenshotWithDelay: function () {
      var doc = this.document();

      setTimeout(function () {
        doc.takeFirstScreenshot();
      }, 1500);
    },

    addTask: function (task) {
      var tasks = this.get("tasks");
      var newTasks = tasks.concat([task]);
      this.set({tasks: newTasks});
      this.updateArrow();
    },

    removeTask: function (task) {
      var tasks = this.get("tasks");
      tasks = _.without(tasks, task);
      this.set({tasks: tasks});
      this.updateArrow();
    },

    tasks: function () {
      return new PageTasks({tasks: this.get("tasks")});
    },

    updateArrow: function () {
      this.set("arrow", undefined, {silent: true});
    },

    arrow: function () {
      if (this.get("arrow") == undefined) {
        var arrow = new PageTasksArrow({
          tasks: this.tasks()
        });

        this.set({arrow: arrow}, {silent: true});
      }

      return this.get("arrow");
    },

    updateArrowPosition: function () {
      if (this.get("arrow") != undefined) {
        this.get("arrow").updatePosition();
      }
    },

    recall: function (f) {
      var self = this;
      this.document().recall(f, function (response) {
        if (response.status == 403) {
          self.trigger("recallfailednotauthorized");
          return false;
        }
        return true;
      });
    }
  });

  });
