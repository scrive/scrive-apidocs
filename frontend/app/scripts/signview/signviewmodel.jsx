var React = require("react");
var Backbone = require("backbone");
var _ = require("underscore");
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var EmailValidation = require("../../js/validation.js").EmailValidation;
var SSNForNOBankIDValidation = require("../../js/validation.js").SSNForNOBankIDValidation;
var SSNForSEBankIDValidation = require("../../js/validation.js").SSNForSEBankIDValidation;
var PhoneValidation = require("../../js/validation.js").PhoneValidation;
var PageTasks = require("../../js/tasks.js").PageTasks;
var PageTasksArrow = require("../../js/tasks.js").PageTasksArrow;

    module.exports = Backbone.Model.extend({
    defaults: {
      hasChangedPin: false,
      hasTakenFirstScreenshot: false,
      hasSentTrackingData: false,
      arrowDisabled: false,
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

      document.setReferenceScreenshot(BrowserInfo.isSmallScreen() ? "mobile" : "standard");

      document.bind("change", function () {
          if (!model.isReady()) {
            model.trigger("change");
          } else if (!triggeredChangeOnReady) { // Once model is ready - we want to trigger change only once
            triggeredChangeOnReady = true;
            document.mainfile().fetch({
              data: {signatory_id: document.currentSignatory() && document.currentSignatory().signatoryid()},
              processData: true,
              cache: false
            });
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
      this.listenTo(this, "change:tasks", this.updateArrowTasks);
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

    hasPadSigning: function () {
      var doc = this.document();
      var sig = doc.currentSignatory();

      return doc.currentSignatory().padDelivery() &&
        doc.isSignedNotClosed() &&
        doc.signatoriesThatCanSignNowOnPad().length > 0;
    },

    allowsavesafetycopy: function () {
      return this.get("allowsavesafetycopy");
    },

    loggedInAsAuthor: function () {
      return this.get("loggedInAsAuthor");
    },

    isReady: function () {
      return this.document().ready() && this.document().mainfile() != undefined;
    },

    hasAnySection: function () {
      return this.isReady()
        || this.hasAuthorAttachmentsSection()
        || this.hasExtraDetailsSection()
        || this.hasSignatoriesAttachmentsSection()
        || this.hasPostSignView()
        || this.hasSignSection();
    },

    hasChangedPin: function () {
      return this.get("hasChangedPin");
    },

    hasRejectOption: function () {
      return this.document().showrejectoption();
    },

    hasSignSection: function () {
      var signatory = this.document().currentSignatory();
      return this.document().currentSignatoryCanSign() && this.hasArrows();
    },

    hasSignatoriesSection: function () {
      return !this.document().closed()
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
      var askForSSN = this.askForSSN();

      return this.askForName()
          || this.askForEmail()
          || this.askForSSNIfNotEID()
          || this.askForPhoneIfNotPin();
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

    askForSSNIfNotEID: function () {
      var document = this.document();
      var signatory = document.currentSignatory();

      return this.askForSSN() && !signatory.seBankIDAuthenticationToSign();
    },

    askForPhone: function () {
      var signatory = this.document().currentSignatory();
      var field = signatory.mobileField();
      return field != undefined
        && !new PhoneValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
    },

    askForPhoneIfNotPin: function () {
      var document = this.document();
      var signatory = document.currentSignatory();

      return this.askForPhone() && !signatory.smsPinAuthenticationToSign();
    },

    hasSignatoriesAttachmentsSection: function () {
      return this.document().currentSignatory().attachments().length > 0;
    },

    hasArrows: function () {
      var file = this.document().mainfile();
      return this.document().ready() && this.document().currentSignatoryCanSign()
        && file.ready() && file.view && file.view.ready();
    },

    hasPostSignView: function () {
      var document = this.document();
      return document.currentSignatory() != undefined
         && !document.currentSignatory().saved()
         && document.currentSignatory().email() // We assume that if this email is set - then it is valid
         && !document.currentSignatory().padDelivery()
         && !document.currentSignatory().apiDelivery()
         && document.currentSignatory().hasSigned()
         && this.allowsavesafetycopy();
    },

    hasDonePostRenderTasks: function () {
      return this.get("hasSentTrackingData") && this.get("hasTakenFirstScreenshot");
    },

    sendTrackingData: function () {
      this.set({hasSentTrackingData: true}, {silent: true});

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
      this.set({hasTakenFirstScreenshot: true}, {silent: true});

      var doc = this.document();

      setTimeout(function () {
        doc.takeFirstScreenshot();
      }, 1500);
    },

    addTask: function (task) {
      var tasks = this.get("tasks");
      var newTasks = tasks.concat([task]);
      this.set({tasks: newTasks});
    },

    removeTask: function (task) {
      var tasks = this.get("tasks");
      tasks = _.without(tasks, task);
      this.set({tasks: tasks});
    },

    updateArrowTasks: function () {
      var arrow = this.arrow();
      var tasks = this.tasks();
      arrow.replaceTasks(tasks);
    },

    tasks: function () {
      return new PageTasks({tasks: this.get("tasks")});
    },

    createArrow: function () {
      var arrow = new PageTasksArrow({
        tasks: this.tasks()
      });

      this._arrow = arrow;
    },

    arrow: function () {
      if (!this._arrow) {
        this.createArrow();
      }

      return this._arrow;
    },

    updateArrowPosition: function () {
      this.arrow().updatePosition();
    },

    updateArrow: function () {
      this.arrow().updateArrow();
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
