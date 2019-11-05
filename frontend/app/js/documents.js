var Backbone = require("backbone");
var moment = require("moment");
var AjaxQueue = require("./ajaxqueue.js").AjaxQueue;
var _ = require("underscore");
var Submit = require("./submits.js").Submit;
var Mail = require("./confirmationsWithEmails.js").Mail;
var File = require("./files.js").File;
var AuthorAttachment = require("./authorattachment.js").AuthorAttachment;
var Signatory = require("./signatories.js").Signatory;
var takeScreenshot = require("./takeScreenshot").takeScreenshot;
var Track = require("../scripts/common/track");

/* Document model
 */


var Document = exports.Document = Backbone.Model.extend({
    defaults: function() { return {
        id: 0,
        title: "",
        signatories: [],
        file: null,
        sealedfile: null,
        authorattachments: [],
        signatoryattachments: [],
        ready: false,
        authentication: "standard",
        delivery: "email",
        template: false,
        timezone : "Europe/Stockholm",
        saveQueue : new AjaxQueue(),
        is_saved : false,
        screenshots : {},
        companyAdmin: false,
        shareable_link: undefined
    }},
    initialize: function(args) {
        this.url = "/api/frontend/documents/" + args.id  + "/get" + (args.siglinkid ? "?signatory_id="+ args.siglinkid : "");
    },
    documentid: function() {
        return this.get("id");
    },
    initialDocumentData: function() {
        return this.get("initialdocumentdata");
    },
    unsetInitialDocumentData: function() {
      this.unset("initialdocumentdata", {silent: true});
    },
    signatories: function() {
        return this.get("signatories");
    },
    signatoriesThatCanSignNow: function() {
        var sigs = _.filter(this.signatories(),function(sig) {return sig.ableToSign()});
        return _.sortBy(sigs,function(sig) {return sig.author()? 2 : 1});
    },
    signatoriesThatCanSignNowOnPad: function() {
        var sigs = _.filter(this.signatories(),function(sig) {return sig.ableToSign() && sig.padDelivery()});
        return _.sortBy(sigs,function(sig) {return sig.author()? 2 : 1});
    },
    signatoriesThatCanSignOrApproveNowOnPad: function() {
        var sigs = _.filter(this.signatories(),function(sig) {return sig.ableToSignOrApprove() && sig.padDelivery()});
        return _.sortBy(sigs,function(sig) {return sig.author()? 2 : 1});
    },
    setDefaultTitle: function () {
      var date = new Date();
      var pad = function (n) { return ("0" + n).slice(-2); };
      var title = localization.designview.newDocumentTitle + " " +
          date.getFullYear() + "-" +
          pad(date.getMonth() + 1) + "-" +
          pad(date.getDate()) + " " +
          pad(date.getHours()) + ":" +
          pad(date.getMinutes());

      this.setTitle(title);
    },
    addExistingSignatory: function(sig) {
        var document = this;
        var signatories = document.signatories();
        signatories[signatories.length] = sig;
        var time = new Date().getTime();
        this.checkLastViewerChange();
        document.trigger('change:signatories');
        document.trigger('change');
    },
    mainfile: function() {
        var file;
        if (this.closed())
            file = this.get("sealedfile");
        else
            file = this.get("file");
        return file;
    },
    authorattachments: function() {
        return this.get("authorattachments");
    },
    removeattachment : function(a) {
        this.set({"authorattachments": _.without(this.authorattachments(), a) });
    },
    signatoryattachments: function() {
        return _.flatten(_.map(this.signatories(), function(sig) { return sig.attachments(); }));
    },
    ready: function() {
        return this.get("ready");
    },
    lang: function() {
        return this.get("lang");
    },
    setLanguage: function(v) {
        this.set("lang",v);
    },
    title: function() {
        return this.get("title");
    },
    setTitle: function(title) {
        this.set({title: title});
    },
    daystosign: function() {
          return this.get("daystosign");
    },
    setDaystosign: function(daystosign) {
         this.set({"daystosign": daystosign});
         if (this.daystoremind() != undefined && this.daystoremind() > daystosign)
           this.setDaystoremind(undefined);
    },
    daystoremind: function() {
          return this.get("daystoremind");
    },
    setDaystoremind: function(daystoremind) {
         var old = this.get("daystoremind");
         this.set({"daystoremind": daystoremind == undefined ? undefined : Math.min(this.daystosign(),daystoremind)});
    },
    showheader: function() {
      return this.get("showheader");
    },
    setShowheader: function(showheader) {
      this.set({"showheader": showheader});
    },
    showrejectoption: function() {
      return this.get("showrejectoption");
    },
    setShowrejectoption: function(showrejectoption) {
      this.set({"showrejectoption": showrejectoption});
    },
    allowrejectreason: function() {
      return this.get("allowrejectreason");
    },
    setAllowrejectreason: function(allowrejectreason) {
      this.set({"allowrejectreason": allowrejectreason});
    },
    showpdfdownload: function() {
      return this.get("showpdfdownload");
    },
    setShowpdfdownload: function(showpdfdownload) {
      this.set({"showpdfdownload": showpdfdownload});
    },
    showfooter: function() {
      return this.get("showfooter");
    },
    setShowfooter: function(showfooter) {
      this.set({"showfooter": showfooter});
    },
    setReferenceScreenshot: function(d) {
      this.get("screenshots").referenceName = d;
    },
    prolongDaysBase: function() {
      if (this.timeouttime() && this.pending()) {
        var timeout = new moment(this.timeouttime());
        return Math.max(0,Math.round(moment.duration(timeout.diff(new moment())).asDays()));
      } else {
        return 0;
      }
    },
    restart: function() {
          return new Submit({
              url: "/api/frontend/documents/" + this.documentid() + "/restart",
              method: "POST"
          });
    },
    prolong: function(days) {
          return new Submit({
              url: "/api/frontend/documents/" + this.documentid() + "/prolong",
              method: "POST",
              days : days
          });
    },
    setautoreminder: function(days) {
          return new Submit({
              url: "/api/frontend/documents/" + this.documentid() + "/setautoreminder",
              method: "POST",
              days : days
          });
    },
    cancel: function() {
          return new Submit({
              url: "/api/frontend/documents/" + this.documentid() + "/cancel",
              method: "POST"
          });
    },
    setInvitationMessage: function(customtext) {
        this.set({invitationmessage: customtext });
    },
    invitationmessage : function() {
        return this.get("invitationmessage");
    },
    inviteMail: function() {
      return new Mail({
        document: this,
        type: "invite",
        editWidth: 300
      });
    },
    setConfirmationMessage: function(customtext) {
      this.set({confirmationmessage: customtext });
    },
    confirmationmessage : function() {
        return this.get("confirmationmessage");
    },
    showArrow: function() {
        return this.get("showarrow");
    },
    setShowArrow: function(showArrow) {
        return this.set({"showarrow": showArrow});
    },
    confirmMail: function() {
      return new Mail({
        document: this,
        type: "confirm",
        editWidth: 300
      });
    },
    takeFirstScreenshot: function() {
        var document = this;
        if (document.file() && document.file().view.readyFirstPage()) {
            document._firstPageHeight = document.file().view.state.images[0].height;
            document.takeScreenshot(true, null);
        } else {
            document.file().on('FirstPageReady', function() {
              document._firstPageHeight = document.file().view.state.images[0].height;
              document.takeScreenshot(true, null);
            });
        }
    },
    takeSigningScreenshot: function(done) {
        this.takeScreenshot(false, done);
    },
    takeScreenshot: function(first, done) {
        var document = this;
        var extraErrorLogParams = {'Number of pages': document.file().pages().length};
        function callDone() {
            $("body").removeClass("screenshot");
            if (done) done();
        }
        $("body").addClass("screenshot");
        takeScreenshot(
            function(canvas) {
                var trackError = function (e) {
                  Track.track("Take screenshot failed",
                              {"Reason": "Canvas exception",
                               "Exception": e,
                               "Browser": $.browser.name,
                               "Browser version": $.browser.version,
                               "Platform": $.browser.platform});
                };
                try {
                  var shot = {"time" : new Date().toISOString(),
                              "image": canvas.toDataURL("image/png")};
                  if (shot.image !== "data:,") {
                    if (first) {
                      document.get("screenshots").first = shot;
                    } else {
                      document.get("screenshots").signing = shot;
                    }
                  } else {
                    trackError("Blank screenshot");
                  }
                } catch(e) {
                  // Some old browsers can throw exception here. We need to catch it, to execute callDone later.
                  trackError(e);
                }
                callDone();
            },
            function(e) { callDone() },
            function() { callDone() },
            20000,
            extraErrorLogParams);
    },
    fieldsForSigning : function() {
        var fields = [];
        _.each(this.currentSignatory().fields(), function(field) {
            if (field.hasDataForSigning()) {
              fields.push(field.dataForSigning());
            }
        });
        return fields;
    },
    consentResponsesForSigning: function() {
      var responses = [];
      var consentModule = this.currentSignatory().consentModule();
      if(consentModule) {
        responses = _.map(consentModule.questions(), function(question) {
          return {
            id: question.questionid(),
            response: question.response()
          };
        });
      }
      return responses;
    },
    acceptedAuthorAttachmentsForSigning : function() {
      var acceptedAttachments = _.filter(this.authorattachments(), function(a) {return a.isAccepted()});
      return _.map(acceptedAttachments,function(a) {
        return a.fileid();
      });
    },
    notUploadedSignatoryAttachments : function() {
      var notUploadedAtts = _.filter(this.currentSignatory().attachments(), function(a) { return a.isMarkedAsNotUploaded() });
      return _.map(notUploadedAtts, function(a) {
        return a.name();
      });
    },
    requestPinToView : function(successCallback,errorCallback) {
      var document = this;
      return new Submit({
          url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/sendsmspintoview",
          method: "POST",
          mobile: document.currentSignatory().mobile(),
          ajax: true,
          ajaxsuccess : successCallback,
          ajaxerror : errorCallback
          });
    },
    requestPinToSign : function(successCallback,errorCallback) {
      var document = this;
      return new Submit({
          url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/sendsmspin",
          method: "POST",
          mobile: document.currentSignatory().mobile(),
          ajax: true,
          ajaxsuccess : successCallback,
          ajaxerror : errorCallback
          });
    },
    identifyToViewWithSMSPin : function(pin,successCallback,errorCallback) {
      var document = this;
      return new Submit({
          url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/smspinidentifytoview",
          method: "POST",
          sms_pin: pin,
          ajax: true,
          ajaxsuccess : successCallback,
          ajaxerror : errorCallback
          });
    },
    cancelSigning: function(successCallback,errorCallback) {
      var document = this;
      var signatory = document.currentSignatory();
      return new Submit({
        url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/signing/cancel",
        method: "POST",
        ajax: true,
        ajaxsuccess : successCallback,
        ajaxerror : errorCallback
      });
    },
    checkingSigning: function(successCallback, inProgressCallback, errorCallback) {
        var document = this;
        var signatory = document.currentSignatory();
        return new Submit({
          url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/signing/check?_=" + Math.random(),
          method: "GET",
          ajax: true,
          ajaxsuccess : function(data, status, xhr) {
            if (data.signed) {
              successCallback();
            } else if (data.in_progress) {
              inProgressCallback(data.last_check_status);
            } else {
              errorCallback(data.last_check_status, xhr);
            }
          },
          ajaxerror : errorCallback
        });
    },
    checksign: function(successCallback, errorCallback,extraSignFields) {
        var document = this;
        var signatory = document.currentSignatory();
        var fields = this.fieldsForSigning();
        var consentResponses = this.consentResponsesForSigning();
        var acceptedAuthorAttachments = this.acceptedAuthorAttachmentsForSigning();
        extraSignFields = extraSignFields || {};
        var problemCallback = function (xhr) {
          Track.track("Check API call failed", {"Reason": xhr.responseText});
          return errorCallback(xhr);
        };
        if (window.checkCallCount === undefined) {
          window.checkCallCount = 1;
        }
        var url = "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/check"
        if (window.checkCallCount++ > 1) {
          // temporary debugging code
          // dummy param, so the backend can see if multiple requests, really came from same tabe.
          url += '?count=' + window.checkCallCount;
        }
        return new Submit({
            url : url,
            method: "POST",
            fields: JSON.stringify(fields),
            consent_responses: JSON.stringify(consentResponses),
            authentication_type: signatory.authenticationToSign(),
            authentication_value: signatory.authenticationToSignFieldValue(),
            accepted_author_attachments: JSON.stringify(acceptedAuthorAttachments),
            not_uploaded_signatory_attachments: JSON.stringify(this.notUploadedSignatoryAttachments()),
            ajax: true,
            ajaxsuccess : successCallback,
            ajaxerror : problemCallback
            }).addMany(extraSignFields);
    },
    sign: function(errorCallback, successCallback,extraSignFields) {
        var document = this;
        var signatory = document.currentSignatory();
        var fields = this.fieldsForSigning();
        var consentResponses = this.consentResponsesForSigning();
        var acceptedAuthorAttachments = this.acceptedAuthorAttachmentsForSigning();
        extraSignFields = extraSignFields || {};
        return new Submit({
            url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/sign",
            method: "POST",
            screenshots: JSON.stringify(document.get("screenshots")),
            fields: JSON.stringify(fields),
            consent_responses: JSON.stringify(consentResponses),
            authentication_type: signatory.authenticationToSign(),
            authentication_value: signatory.authenticationToSignFieldValue(),
            accepted_author_attachments: JSON.stringify(acceptedAuthorAttachments),
            not_uploaded_signatory_attachments: JSON.stringify(this.notUploadedSignatoryAttachments()),
            ajax: true,
            ajaxsuccess : function(newDocumentRaw) {
              newDocument = new Document(new Document({}).parse(newDocumentRaw)),
              oldDocument = document;
	      successCallback(newDocument, oldDocument);
            },
            ajaxerror : function(xhr) {
              if (errorCallback != undefined) {
                // this is temporary
                if (xhr.status === 400) {
                  var image = document.get("screenshots").first.image;
                  if (typeof image === "string") {
                    image = image.substr(0, 50);
                  }
                  Track.track('Sign call failed', {"first_screenshot": image});
                }
                errorCallback(xhr);
              } else {
                window.location.reload();
              }
            }
        }).addMany(extraSignFields);
    },
    approve : function(errorCallback, successCallback) {
        var document = this;
        return new Submit({
            url : "/api/frontend/documents/" + document.documentid() +  "/" + document.currentSignatory().signatoryid() + "/approve",
            method: "POST",
            ajax: true,
            ajaxsuccess : function(newDocumentRaw) {
              newDocument = new Document(new Document({}).parse(newDocumentRaw)),
              oldDocument = document;
              successCallback(newDocument, oldDocument);
            },
            ajaxerror : function(xhr) {
              if (errorCallback != undefined) {
                errorCallback(xhr);
              } else {
                window.location.reload();
              }
            }
        });
    },
    clone : function(callback) {
       var document = this;
         return new Submit({
              url : "/api/frontend/documents/" + this.documentid() + "/clone",
              method: "POST",
              ajaxtimeout : 120000,
              ajaxsuccess : function(resp) {
                var nd = new Document({id : resp.id, screenshots : document.get("screenshots") }); // Note that we are not cloning screenshot object, as it may be too big.
                nd.set(nd.parse(resp));
                callback(nd);
              }
          });
    },
    makeReadyForSigning : function() {
         var document = this;
         return new Submit({
              url : "/api/frontend/documents/" + this.documentid() + "/start",
              method: "POST",
              ajaxtimeout : 120000
          });
    },
    save: function(callback) {
         this.get("saveQueue").add(new Submit({
              url: "/api/frontend/documents/" + this.documentid() +"/update",
              method: "POST",
              document: JSON.stringify(this.draftData()),
              ajaxsuccess : function() {if (callback != undefined) callback();}
         }), function(ec) {if (ec === 403 || ec === 401) window.location.reload()});
    },
    afterSave: function(f) {
        this.get("saveQueue").finishWith(f);
    },
    saved : function() {
        return this.get("is_saved");
    },
    setSaved : function() {
        this.set({is_saved:true});
    },
    setAttachments: function() {
        return new Submit({
              url: "/api/frontend/documents/"+this.documentid()+"/setattachments",
              method: "POST",
              ajax: true
          });
    },

    setAttachmentsIncrementally: function() {
      // Not using Submit as there is no way to create an <input type="file">
      // with a custom FileList even if we already have the object of type File.

      var data = new FormData();
      data.append("xtoken", Cookies.getMulti("xtoken").join(";"));
      data.append("incremental", true);

      var xhr = new XMLHttpRequest();
      var url = "/api/frontend/documents/" + this.documentid() + "/setattachments";
      xhr.open("POST", url, true);

      // Mimics the interface of Submit
      return {
        add: function(name, value) {
          return data.append(name, value);
        },

        sendAjax: function(onSuccess, onError) {
          xhr.onload = function(event) {
            if(xhr.status == 200) {
              onSuccess(xhr);
            } else {
              onError(xhr);
            }
          };
          xhr.send(data);
        }
      };
    },

    setHighlight : function(pageno, imageData, callback) {
       var doc = this;
       var sig = doc.currentSignatory();
       var currentHighlight = _.find(sig.highlightedPages(), function(hp) { return hp.page() == pageno; });
       return new Submit({
        method: "POST",
        ajax: true,
        url : "/api/frontend/documents/"+ doc.documentid() + "/" + sig.signatoryid() + "/signing/highlight",
        page : "" + pageno,
        image : imageData,
        ajaxsuccess: function (resp) {
          var newCurrentSig = _.find(resp.parties, function(p) {return p.id == sig.signatoryid();});
          var newHighlight = _.find(newCurrentSig.highlighted_pages, function(p) {return p.page == pageno;});
          if (currentHighlight && newHighlight) {
            sig.updateHighlightedPage(pageno, newHighlight.file_id);
          } else if (!currentHighlight && newHighlight) {
            sig.addHighlightedPage(pageno, newHighlight.file_id);
          } else if (currentHighlight && !newHighlight) {
            sig.removeHighlightedPage(pageno);
          }
          if (callback) {
            callback();
          }
        }
      });
    },
    draftData: function() {
      return {
          id: this.documentid(),
          title: this.title(),
          invitation_message: this.get("invitationmessage"),
          confirmation_message: this.get("confirmationmessage"),
          days_to_sign: this.get("daystosign"),
          days_to_remind: this.get("daystoremind") != undefined ? this.get("daystoremind") : null,
          parties: _.map(this.signatories(), function(sig) {return sig.draftData()}),
          lang: this.lang(),
          is_template: this.isTemplate(),
          is_saved: this.get("is_saved"),
          timezone : this.get("timezone"),
          display_options : {
            show_header: this.get("showheader"),
            show_pdf_download: this.get("showpdfdownload"),
            show_reject_option: this.get("showrejectoption"),
            allow_reject_reason: this.get("allowrejectreason"),
            show_footer: this.get("showfooter"),
            document_is_receipt: this.get("documentisreceipt"),
            show_arrow: this.get("showarrow")
          }
      };
    },
    status: function() {
          return this.get("status");
    },
    currentSignatory: function() {
       return _.detect(this.signatories(), function(signatory) {
           return signatory.current();
        });
    },
    removeSignatory: function(sig) {
       if (this.signatories().length < 2)
           return;
       var newsigs = new Array();
       newsigs.push(this.signatories()[0]);
       var removed = false;
       for (var i = 1; i < this.signatories().length; i++)
          if ((sig !== this.signatories()[i] && i < this.signatories().length - 1) ||
                 removed)
             newsigs.push(this.signatories()[i]);
          else
          {   this.signatories()[i].removed();
              removed = true;
          }
       this.set({signatories : newsigs});
       this.fixSignorderAfterRemoving(sig);
       this.checkLastViewerChange();
       this.trigger("change:signatories");
       this.trigger("change");
    },
    currentViewerIsAuthor: function() {
        var csig = this.currentSignatory();
        return (csig != undefined && csig.author());
    },
    currentViewerIsAuthorsCompanyAdmin : function() {
        return this.get("companyAdmin");
    },
    preparation: function() {
        return this.status() == "preparation";
    },
    pending: function() {
        return this.status() == "pending";
    },
    timedout: function() {
        return this.status() == "timedout";
    },
    canceled: function() {
        return this.status() == "canceled";
    },
    rejected: function() {
        return this.status() == "rejected";
    },
    closed: function() {
        return this.status() == "closed";
    },
    timeouttime: function() {
        return this.get("timeouttime");
    },
    autoremindtime: function() {
        return this.get("autoremindtime");
    },
    file: function() {
        return this.get("file");
    },
    signorder: function() {
      // do not use this.maxPossibleSignOrder() because it's possible
      // doc was set up through api with signorder 'holes'
      // (e.g. signorders: 1, 2, 4)
      var max = _.max(this.signatories(), function (s) { return s.signorder(); }).signorder();
      var res = max;
      _.each(this.signatories(), function (s) {
        if ((s.signs() || s.approves()) && !s.hasSigned()) {
          res = Math.min(res, s.signorder());
        }
      });
      return res;
    },
    isTemplate: function() {
       return this.get("template") == true;
    },
    makeTemplate: function() {
       return this.set({"template": true});
    },
    // errorCallback(response) should return bool value
    // indicating if further attempts should be continued
    recall: function(successCallback, errorCallback) {
        var self = this;
        var fetchOptions = {
                             processData: true,
                             cache: false };
        var fetchFunction = function () {
            self.fetch(fetchOptions);
        };
        fetchOptions.error = function(model, response) {
            if (errorCallback === undefined || errorCallback(response) === true) {
              console.error("Failed to fetch document #" + self.documentid() + ", trying again in one sec...");

              // if user is logged out, there's no point in trying again
              // or if status is 0, whatever that means it will not fix itself
              // or if this tab was loaded for so long that we have deleted the draft
              if (response.status !== 401 && response.status !== 0 && response.status !== 404) {
                window.setTimeout(fetchFunction, 1000);
              }
            } else {
              console.error("Failed to fetch document #" + self.documentid() + ", giving up.");
            }
        };
        fetchOptions.success = function() {
          _.each(self.signatories(), function(sig) {
            sig.ensureAllFields();
          });
          if (successCallback !== undefined) {
            successCallback();
          }
        };

        // See if we have document data in the object
        var initialDocumentData = this.initialDocumentData();
        if (initialDocumentData) {
          this.unsetInitialDocumentData();
          this.set(this.parse(initialDocumentData));
          fetchOptions.success();
        } else {
          fetchFunction();
        }
    },
    author: function() {
        return _.find(this.signatories(),
                      function(s) { return s.author(); });
    },
    authorCanSignFirst : function() {
        if (!this.author().signs())
            return false;
        if (this.author().hasPlacedSignatures()) {
            // We don't support drawing signature in design view
            return false;
        }
        if (this.author().hasPlacedRadioGroups()) {
          // If the author has any radiogroups we have to direct them to
          // the sign view.
          return false;
        }
        if (this.author().seBankIDAuthenticationToSign() || this.author().seBankIDAuthenticationToView()
          ||this.author().noBankIDAuthenticationToSign() || this.author().noBankIDAuthenticationToView()
          ||this.author().dkNemIDAuthenticationToSign()  || this.author().dkNemIDAuthenticationToView()
          ||this.author().fiTupasAuthenticationToView()) {
            // We don't support eleg authorization in design view
            return false;
        }

        if (this.author().smsPinAuthenticationToView() || this.author().smsPinAuthenticationToSign()) {
            // We don't support sms pin for author from design view
            return false;
        }

        var aidx = this.author().signorder();
        return ! _.any(this.signatories(), function(sig) {
            return (sig.signs() || sig.approves()) && sig.signorder() < aidx;
            });

    },
    authorIsOnlySignatory : function() {
       return this.author().signs() && _.all(this.signatories(), function(sig) {
           return !sig.author() && !sig.signs() && !sig.approves();
       })
    },
    isSigning: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && this.pending() && signatory.signs() && !signatory.hasSigned();
    },
    isApproving: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && this.pending() && signatory.approves() && !signatory.hasSigned();
    },
    isReviewing: function() {
        var signatory = this.currentSignatory();
        return (signatory != undefined)
            && (this.pending() || this.closed())
            && !signatory.signs() && !signatory.approves();
    },
    isSignedNotClosed: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && this.pending() && signatory.hasSigned();
    },
    isSignedAndClosed: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && signatory.hasSigned() && this.closed();
    },
    isUnavailableForSignOrApprove: function() {
        return !this.pending() && !this.closed();
    },
    currentSignatoryCanSign: function() {
      return this.currentSignatory() && this.currentSignatory().canSign();
    },
    currentSignatoryCanSignOrApprove: function() {
      return this.currentSignatory() && this.currentSignatory().canSignOrApprove();
    },
    signatoriesWhoSign: function() {
        return _.filter(this.signatories(), function(sig) {
            return sig.signs();
        });
    },
    maxPossibleSignOrder : function() {
      return this.signatories().length;
    },
    fixSignorderAfterRemoving : function(old) {
      if (!_.any(this.signatories(), function(s) {
        return s.signorder() == old.signorder();
      })) {
        _.each(this.signatories(), function(s) {
          if (s.signorder() > old.signorder()) {
            s.setSignOrder(s.signorder() - 1);
          }
        });
      }
    },
    checkLastViewerChange : function() {
      _.each(this.signatories(), function(s) {
        s.checkLastViewerChange();
      });
    },
    needRecall : function() {
      return this.documentid() != 0 && this.closed() && this.mainfile() == undefined;
    },
    parse: function(args) {
     var self = this;

     setTimeout(function() {
         if (self.needRecall())
            self.recall();
     },500);
     var dataForFile =
        { documentid: self.documentid(),
          document: self
        };
     if (self.file() != undefined) self.file().off();

     var signatories = args.parties || [];
     signatories =_.filter(signatories, function(s) { return s.role != "forwarded_party"; });
     return {
       id: args.id,
       title: args.title,
       file: function() {
           if (args.file) {
               var file = new File(_.defaults(args.file, dataForFile));
               file.on('ready', function() {
                   self.trigger('file:change');
               });
               return file;
           } else {
               return null;
           }
       }(),
       sealedfile: args.sealed_file
                      ? new File(_.defaults(args.sealed_file, dataForFile))
                      : null,
       authorattachments: _.map(args.author_attachments, function(fileargs) {
         return new AuthorAttachment(_.defaults(fileargs, dataForFile));
       }),
       signatories: _.map(signatories, function(signatoryargs) {
         return new Signatory(_.defaults(signatoryargs, { document: self , current: signatoryargs.id === args.viewer.signatory_id}));
       }),
       lang: args.lang,
       status: args.status,
       timeouttime: args.timeout_time == undefined ? undefined : moment(args.timeout_time).toDate(),
       autoremindtime: args.auto_remind_time == undefined ? undefined : moment(args.auto_remind_time).toDate(),
       template: args.is_template,
       daystosign: args.days_to_sign,
       daystoremind: args.days_to_remind,
       showheader: args.display_options.show_header,
       showpdfdownload: args.display_options.show_pdf_download,
       showrejectoption: args.display_options.show_reject_option,
       allowrejectreason: args.display_options.allow_reject_reason,
       documentisreceipt:  args.display_options.document_is_receipt,
       showfooter: args.display_options.show_footer,
       showarrow: args.display_options.show_arrow,
       invitationmessage: args.invitation_message,
       confirmationmessage: args.confirmation_message,
       timezone: args.timezone,
       is_saved: args.is_saved,
       companyAdmin: args.viewer.role == "company_admin",
       ready: true,
       shareableLink: args.shareable_link
     };
    },
    markAsNotReady: function() {
        this.set({ready:false});
    },
    // validation
    hasProblems: function() {
        return this.hasDocumentProblems() || this.hasSignatoryProblems();
    },
    hasDocumentProblems: function() {
        return !this.hasAtLeastOneSignatory() ||
            !this.mainfile();
    },
    hasAtLeastOneSignatory: function() {
        var signing = this.signatoriesWhoSign();
        return signing.length >= 1;
    },
    hasSignatoryProblems: function() {
        var sigs = this.signatories();
        return _.some(sigs, function(sig) {
            return sig.hasProblems();
        });
    },
    newNameWithIndex: function (prefix) {
        var allnumbers = [];
        _.each(this.signatories(), function(s) {
            _.each(s.fields(), function(f) {
                var name = f.name();
                var match = /(\d+)$/.exec(name);
                if (name.indexOf(prefix) > -1 && match && match[1]) {
                  allnumbers.push(parseInt(match[1], 10));
                }
            });
        });

        var i = 1;
        while (_.contains(allnumbers, i)) {
          i++;
        }

        return prefix + ' ' + i;
    },
    newCheckboxName: function() {
        return this.newNameWithIndex(localization.designview.checkbox);
    },
    newSignatureName: function() {
        return this.newNameWithIndex(localization.designview.signature);
    },
    newCustomName: function() {
        return this.newNameWithIndex(localization.designview.customField);
    },
    newRadioGroupName: function() {
        return this.newNameWithIndex(localization.designview.radiogroup);
    },
    allPlacements: function() {
        var document = this;
        var out = [];
        _.each(document.signatories(), function(sig) {
            _.each(sig.fields(), function(field) {
                _.each(field.placements(), function(placement) {
                    out.push(placement);
                });
            });
        });
        return out;
    },
    killAllPlacements: function() {
        var document = this;
        _.each(document.signatories(), function(sig) {
            _.each(sig.fields(), function(field) {
                _.each(field.placements(), function(placement) {
                    placement.die();
                });
            });
        });

    },
    hasEleg: function() {
        return _.some(this.signatories(), function(s) {
            return  (s.seBankIDAuthenticationToSign() || s.seBankIDAuthenticationToView()
                   ||s.noBankIDAuthenticationToSign() || s.noBankIDAuthenticationToView()
                   ||s.dkNemIDAuthenticationToSign()  || s.dkNemIDAuthenticationToView()
                   ||s.fiTupasAuthenticationToView());
        });
    },
    hasEmail: function() {
        return _.some(this.signatories(), function(s) {
            return s.emailDelivery() || s.emailMobileDelivery();
        });
    },
    hasSMS: function() {
        return _.some(this.signatories(), function(s) {
            return s.mobileDelivery() || s.emailMobileDelivery();
        });
    },
    hasPad: function() {
        return _.some(this.signatories(), function(s) {
            return s.padDelivery();
        });
    },
    hasEmailAndSMS: function() {
        return _.some(this.signatories(), function(s) {
            return s.emailMobileDelivery();
        });
    },
    isCsv : function() {
      return _.any(this.signatories(),function(s) {
          return s.isCsv();
      });
    },
    csv : function() {
      var csv = undefined;
      _.each(this.signatories(),function(s) {
        csv = csv || s.csv();
      });
      return csv;
    },
    normalizeWithFirstCSVLine : function() {
      var name;
      _.each(this.signatories(),function(s) {
          if (s.isCsv())
           name =  s.normalizeWithFirstCSVLine();
      });
      return name;
    },
    dropFirstCSVLine : function() {
      _.each(this.signatories(),function(s) {
          if (s.isCsv())
            s.dropFirstCSVLine();
      });
    },
    adjustAuthorFieldsAfterCloning : function(odoc) {
      author = this.author();
      oauthor = odoc.author();
      if (author.personalnumberField() && oauthor.personalnumberField()) {
        author.personalnumberField().setValue(oauthor.personalnumberField().value());
      }
      if (author.mobileField() && oauthor.mobileField()) {
        author.mobileField().setValue(oauthor.mobileField().value());
      }
      if (author.companyField() && oauthor.companyField()) {
        author.companyField().setValue(oauthor.companyField().value());
      }
      if (author.companynumberField() && oauthor.companynumberField()) {
        author.companynumberField().setValue(oauthor.companynumberField().value());
      }
    },
    allHighlighedPagesForPageNo: function(pageno) {
      return _.flatten(_.map(this.signatories(), function(s) {
        return _.filter(s.highlightedPages(), function(hp) { return hp.page() == pageno; })
      }));
    },
    noneditableHighlighedPagesForPageNo: function(pageno) {
      return _.filter(this.allHighlighedPagesForPageNo(pageno), function(hp) { return  hp.signatory().hasSigned();});
    },
    editableHighlighedPageForPageNo: function(pageno) {
      if (this.currentSignatoryCanSign()) {
        return _.find(this.currentSignatory().highlightedPages(), function(hp) { return  hp.page() == pageno;});
      } else {
        return undefined;
      }
    },

    shareableLink: function() {
      return this.get("shareable_link");
    },

    generateShareableLink: function(successCallback, errorCallback) {
      const self = this;

      return new Submit({
        method: "POST",
        ajax: true,
        url: "/api/v2/documents/" + this.documentid() + "/shareablelink/generate",
        ajaxsuccess: function (resp) {
          self.set("shareable_link", resp.shareable_link);
          successCallback(resp);
        },
        ajaxerror: errorCallback
      });
    },

    discardShareableLink: function(successCallback, errorCallback) {
      const self = this;

      return new Submit({
        method: "POST",
        ajax: true,
        url: "/api/v2/documents/" + this.documentid() + "/shareablelink/discard",
        ajaxsuccess: function (resp) {
          self.set("shareable_link", null);
          successCallback(resp);
        },
        ajaxerror: errorCallback
      });
    },

    hasSeveralFiles: function () {
      const attachments = this.authorattachments();

      if(this.closed()) {
        return _.any(attachments, function(att) {
          return !att.isAddToSealedFile();
        });
      } else {
        const has_sig_atts = _.any(this.signatories(), function (sig) {
          return _.any(sig.attachments(), function (att) {
            return att.hasFile();
          });
	});
        return attachments.length > 0 || has_sig_atts;
      }
    },

    downloadLink: function (asDownload) {
      const isZip = this.hasSeveralFiles();

      if (isZip) {
        name = name.replace(/\.pdf$/i, ".zip");
      }

      const link = "/api/frontend/documents/" + this.documentid() + "/files/"
        + (isZip ? "zip" : "main") + "/" + encodeURIComponent(this.title())
        + "." + (isZip ? "zip" : "pdf") + "?as_download=" + !!asDownload
        + (this.currentSignatory()
           ? "&signatory_id=" + this.currentSignatory().signatoryid() : "");

      return {
        link: link,
        isZip: isZip
      };
    }
});
