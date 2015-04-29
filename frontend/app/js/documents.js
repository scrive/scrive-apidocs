/* Document model
 * Also document viewer (person thet is looking at document so we can hold current signatory link id somewere)
 */

define(['Backbone', 'moment', 'legacy_code'], function(Backbone, moment) {

window.DocumentViewer = Backbone.Model.extend({
    authorcompanyadmin : function() {
        return this.get("authorcompanyadmin");
    },
    signatoryid: function() {
      return this.get("signatoryid");
    },
    urlPart: function() {
        if (this.signatoryid() != undefined)
          return "?signatorylinkid=" + this.signatoryid();
        else return "";
    },

    forFetch: function() {
        return {
            signatoryid: this.signatoryid()
        };
    }
});

window.Document = Backbone.Model.extend({
    defaults: function() { return {
        id: 0,
        title: "",
        signatories: [],
        file: null,
        sealedfile: null,
        authorattachments: [],
        signatoryattachments: [],
        ready: false,
        readOnlyView: false,
        viewer: new DocumentViewer(),
        infotext: "",
        authentication: "standard",
        delivery: "email",
        template: false,
        timezone : "Europe/Stockholm",
        saveQueue : new AjaxQueue(),
        saved : false,
        screenshots : {}
    }},
    initialize: function(args) {
        this.url = "/api/frontend/get/" + args.id ;
    },
    viewer: function() {
        if (this.get("viewer") != undefined)
            return this.get("viewer");
        else
            return new DocumentViewer(); // Fix for strande backbone behavior
    },
    signed: function() {
        return this.get("signed");
    },
    documentid: function() {
        return this.get("id");
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
    hasDefaultTitle: function () {
      var title = this.title();
      if (title === "") return true;
      var untitled = localization.designview.newDocumentTitle;
      var re = new RegExp(untitled + " \\d{4}-\\d{2}-\\d{2}");
      return re.test(title);
    },
    addExistingSignatory: function(sig) {
        var document = this;
        var signatories = document.signatories();
        signatories[signatories.length] = sig;
        var time = new Date().getTime();
        this.checkLastViewerChange();
        document.trigger('change:signatories');
        document.trigger('change:signorder');
        document.trigger('change');
    },
    authorSignsFirstMode : function() {
      return _.all(this.signatories(), function(sig) {
        return (sig.signs() && sig.author() && sig.signorder() == 1) ||
         (sig.signs() && !sig.author() && sig.signorder() == 2) ||
         (!sig.signs() && !sig.author());
     });
    },
    authorSignsLastMode : function() {
      return _.all(this.signatories(), function(sig) {
        return (sig.signs() && sig.author() && sig.signorder() == 2) ||
          (sig.signs() && !sig.author() && sig.signorder() == 1) ||
          (!sig.signs() && !sig.author());
      });
    },
    authorNotSignsMode : function() {
      return _.all(this.signatories(), function(sig) {
        return (sig.signs() && !sig.author() && sig.signorder() == 1) ||
          (!sig.signs());
      });
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
    hasAnyAttachments: function() {
        return this.authorattachments().length > 0 || this.signatoryattachments().length > 0;
    },
    ready: function() {
        return this.get("ready");
    },
    readOnlyView: function() {
        return this.get("readOnlyView");
    },
    lang: function() {
        return this.get("lang");
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
      this.get("screenshots").reference = d;
    },
    hasReferenceScreenshot : function() {
      return this.get("screenshots").reference != undefined;
    },
    infotext: function() {
        return this.get("infotext");
    },
    canberestarted: function() {
        return this.get("canberestarted");
    },
    canbeprolonged: function() {
        return this.get("canbeprolonged");
    },
    restart: function() {
          return new Submit({
              url: "/api/frontend/restart/" + this.documentid(),
              method: "POST"
          });
    },
    prolong: function(days) {
          return new Submit({
              url: "/api/frontend/prolong/" + this.documentid(),
              method: "POST",
              timezone: jstz.determine().name(),
              days : days
          });
    },
    setautoreminder: function(days) {
          return new Submit({
              url: "/api/frontend/setautoreminder/" + this.documentid(),
              method: "POST",
              days : days
          });
    },
    canbecanceled: function() {
        return this.get("canbecanceled");
    },
    cancel: function() {
          return new Submit({
              url: "/api/frontend/cancel/" + this.documentid(),
              method: "POST"
          });
    },
    cancelMail: function() {
              return new Mail({
                        document: this,
                        type: "cancel"
            });
    },
    canseeallattachments: function() {
      return this.get("canseeallattachments");
    },
    setInvitationMessage: function(customtext)
    {
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
    setConfirmationMessage: function(customtext)
    {
        this.set({confirmationmessage: customtext });
    },
    confirmationmessage : function() {
        return this.get("confirmationmessage");
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
        if (document.file() && document.file().view.readyFirstPage())
            document.takeScreenshot(true, null);
        else
            document.file().on('FirstPageReady', function() {
            document.takeScreenshot(true, null);
            });
    },
    takeSigningScreenshot: function(done) {
        this.takeScreenshot(false, done);
    },
    takeScreenshot: function(first, done) {
        var document = this;
        var extraErrorLogParams = {'Number of pages': document.file().pages().length};
        function callDone() {
            if (done) done();
        }
        window.takeScreenshot(
            function(canvas) {
                try {
                  var shot = { "time" : new Date().toISOString(),
                             "image": canvas.toDataURL("image/jpeg",0.7) };
                  if (first)
                      document.get("screenshots").first = shot;
                  else
                      document.get("screenshots").signing = shot;
                } catch(e) {
                  // Some old browsers can throw exception here. We need to catch it, to execute callDone later.
                  mixpanel.track('Take screenshot failed',{
                    'Reason': "Canvas exception",
                    'Exception': e,
                    'Browser': $.browser.name,
                    'Browser version': $.browser.version,
                    'Platform': $.browser.platform
                  });
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
            if (field.isClosed()) return;
            fields.push({name: field.name(), value: field.value(), type: field.type()});
        });
        return fields;
    },
    requestPin : function(successCallback,errorCallback) {
        var document = this;
        return new Submit({
            url : "/api/frontend/sendsmspin/" + document.documentid() +  "/" + document.currentSignatory().signatoryid(),
            method: "POST",
            phone: document.currentSignatory().mobile(),
            ajax: true,
            expectedType : "text",
            ajaxsuccess : successCallback,
            ajaxerror : errorCallback
            });
    },
    checksign: function(successCallback, errorCallback,extraSignFields) {
        var document = this;
        var signatory = document.currentSignatory();
        var fields = this.fieldsForSigning();
        extraSignFields = extraSignFields || {};
        return new Submit({
            url : "/api/frontend/checksign/" + document.documentid() +  "/" + document.currentSignatory().signatoryid(),
            method: "POST",
            fields: JSON.stringify(fields),
            authentication_type: signatory.authentication(),
            authentication_value: signatory.authenticationFieldValue(),
            ajax: true,
            expectedType : "text",
            ajaxsuccess : successCallback,
            ajaxerror : errorCallback
            }).addMany(extraSignFields);
    },
    sign: function(errorCallback, successCallback,extraSignFields) {
        var document = this;
        var signatory = document.currentSignatory();
        var fields = this.fieldsForSigning();
        extraSignFields = extraSignFields || {};
        return new Submit({
            url : "/api/frontend/sign/" + document.documentid() +  "/" + document.currentSignatory().signatoryid(),
            method: "POST",
            screenshots: JSON.stringify(document.get("screenshots")),
            fields: JSON.stringify(fields),
            authentication_type: signatory.authentication(),
            authentication_value: signatory.authenticationFieldValue(),
            ajax: true,
            expectedType : "text",
            ajaxsuccess : function(newDocumentRaw) {
	      var newDocumentJSON = JSON.parse(newDocumentRaw),
              newDocument = new Document(new Document({}).parse(newDocumentJSON)),
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
        }).addMany(extraSignFields);
    },
    clone : function(callback) {
       var document = this;
         return new Submit({
              url : "/api/frontend/clone/" + this.documentid(),
              method: "POST",
              ajaxtimeout : 120000,
              ajaxsuccess : function(resp) {
                var jresp = JSON.parse(resp);
                var nd = new Document({id : jresp.id, screenshots : document.get("screenshots") }); // Note that we are not cloning screenshot object, as it may be too big.
                nd.set(nd.parse(jresp));
                console.log(nd.get("screenshots"));
                callback(nd);
              }
          });
    },
    makeReadyForSigning : function() {
         var document = this;
         return new Submit({
              url : "/api/frontend/ready/" + this.documentid(),
              timezone: jstz.determine().name(),
              method: "POST",
              expectedType : "json",
              screenshots: JSON.stringify(document.get("screenshots")),
              ajaxtimeout : 120000
          });
    },
    save: function(callback) {
         this.get("saveQueue").add(new Submit({
              url: "/api/frontend/update/" + this.documentid(),
              method: "POST",
              json: JSON.stringify(this.draftData()),
              ajaxsuccess : function() {if (callback != undefined) callback();}
         }), function(ec) {if (ec == 403) window.location.reload()});
    },
    afterSave: function(f) {
        this.get("saveQueue").finishWith(f);
    },
    saved : function() {
        return this.get("saved");
    },
    setSaved : function() {
        this.set({saved:true});
    },
    setAttachments: function() {
        return new Submit({
              url: "/api/frontend/setattachments/" + this.documentid(),
              method: "POST",
              ajax: true
          });
    },
    draftData: function() {
      return {
          title: this.title(),
          invitationmessage: _.escape(this.get("invitationmessage")),
          confirmationmessage: _.escape(this.get("confirmationmessage")),
          daystosign: this.get("daystosign"),
          daystoremind: this.get("daystoremind") != undefined ? this.get("daystoremind") : null,
          apicallbackurl : this.get("apicallbackurl"),
          signatories: _.map(this.signatories(), function(sig) {return sig.draftData()}),
          lang: this.lang().draftData(),
          template: this.isTemplate(),
          authorattachments : _.map(this.authorattachments(), function(a) {return a.draftData()}),
          showheader: this.get("showheader") != undefined ? this.get("showheader") : null,
          showpdfdownload: this.get("showpdfdownload") != undefined ? this.get("showpdfdownload") : null,
          showrejectoption: this.get("showrejectoption") != undefined ? this.get("showrejectoption") : null,
          showfooter: this.get("showfooter") != undefined ? this.get("showfooter") : null,
          saved: this.get("saved"),
          timezone : this.get("timezone")
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
    otherSignatories: function() {
       return _.select(this.signatories(), function(signatory) {
           return !signatory.current();
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
       this.trigger("change:signorder");
    },
    currentViewerIsAuthor: function() {
        var csig = this.currentSignatory();
        return (csig != undefined && csig.author());
    },
    currentViewerIsAuthorsCompanyAdmin : function() {
        return this.viewer().authorcompanyadmin() == true;
    },
    preparation: function() {
        return this.status() == "Preparation";
    },
    pending: function() {
        return this.status() == "Pending";
    },
    timedout: function() {
        return this.status() == "Timedout";
    },
    canceled: function() {
        return this.status() == "Canceled";
    },
    rejected: function() {
        return this.status() == "Rejected";
    },
    closed: function() {
        return this.status() == "Closed";
    },
    signingInProcess: function() {
        return this.pending();
    },
    timeouttime: function() {
        return this.get("timeouttime");
    },
    autoremindtime: function() {
        return this.get("autoremindtime");
    },
    file: function()
    {
        return this.get("file");
    },
    signorder: function() {
      return this.get("signorder");
    },
    elegTBS: function() {
        var text = this.title() + " " + this.documentid();
        _.each(this.signatories(), function(signatory) {
            text += " " + signatory.name() + " " + signatory.personalnumber();
        });
        return text;
    },
    lastSignatoryLeft: function() {
        return _.all(this.signatories(), function(signatory) {
          return (signatory.signs() && signatory.hasSigned()) || !signatory.signs() || signatory.current();
        });
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
        console.log('recall');
        var self = this;
        var fetchOptions = { data: self.viewer().forFetch(),
                             processData: true,
                             cache: false };
        var fetchFunction = function () {
            self.fetch(fetchOptions);
        };
        fetchOptions.error = function(model, response) {
            if (errorCallback === undefined || errorCallback(response) === true) {
              console.error("Failed to fetch document #" + self.documentid() + ", trying again in one sec...");
              window.setTimeout(fetchFunction, 1000);
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

        fetchFunction();

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
        if (this.author().elegAuthentication()) {
            // We don't support eleg authorization in design view
            return false;
        }

        if (this.author().smsPinAuthentication()) {
            // We don't support sms pin for author from design view
            return false;
        }

        var aidx = this.author().signorder();
        return ! _.any(this.signatories(), function(sig) {
            return sig.signs() && sig.signorder() < aidx;
            });

    },
    authorIsOnlySignatory : function() {
       for (var i = 0; i < this.signatories().length; ++i)
         if (this.signatories()[i].signs() && !this.signatories()[i].author())
                    return false;
       return this.author().signs();
    },
    allowsDD: function() {
        return this.preparation();
    },
    isSigning: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && this.signingInProcess() && signatory.signs() && !signatory.hasSigned();
    },
    isReviewing: function() {
        var signatory = this.currentSignatory();
        return (signatory != undefined) && (this.signingInProcess() || this.closed()) && !signatory.signs();
    },
    isSignedNotClosed: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && this.signingInProcess() && signatory.hasSigned();
    },
    isSignedAndClosed: function() {
        var signatory = this.currentSignatory();
        return signatory != undefined && signatory.hasSigned() && this.closed();
    },
    isUnavailableForSign: function() {
        return !this.signingInProcess() && !this.closed();
    },
    currentSignatoryCanSign: function() {
      return this.currentSignatory() && this.currentSignatory().canSign();
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
          signatoryid: self.viewer().signatoryid()
        };
     if (self.file() != undefined) self.file().off();
     var signatories = args.signatories || [];
     var maxSignsSignorder = _.max(signatories, function(s) {
         return s.signs ? s.signorder : 0;
       }).signorder;

     var dropHTMLWrapperFromMessage = function(msg) {
       // Backward compatibility for V1. We are still returning HTML strucure from API for invitation and confirmation.
       return _.unescape(msg.replace("<p>","").replace("</p>",""));

     };

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
       sealedfile: args.sealedfile
                      ? new File(_.defaults(args.sealedfile, dataForFile))
                      : null,
       authorattachments: _.map(args.authorattachments, function(fileargs) {
         return new File(_.defaults(fileargs, dataForFile));
       }),
       signatories: _.map(signatories, function(signatoryargs) {
         return new Signatory(_.defaults(signatoryargs, { document: self, isLastViewer : !signatoryargs.signs && signatoryargs.signorder > maxSignsSignorder }));
       }),
       lang: (function() {
           var lang = new DocLang({lang : args.lang});
           lang.on('change', function() {
               self.trigger('change');
           });
           return lang;
       }()),
       infotext: args.infotext,
       canberestarted: args.canberestarted,
       canbeprolonged: args.canbeprolonged,
       canbecanceled: args.canbecanceled,
       canseeallattachments: args.canseeallattachments,
       status: args.status,
       timeouttime: args.timeouttime == undefined ? undefined : moment(args.timeouttime).toDate(),
       autoremindtime: args.autoremindtime == undefined ? undefined : moment(args.autoremindtime).toDate(),
       signorder: args.signorder,
       authentication: args.authentication,
       delivery: args.delivery,
       template: args.template,
       daystosign: args.daystosign,
       daystoremind: args.daystoremind,
       showheader: args.showheader,
       showpdfdownload: args.showpdfdownload,
       showrejectoption: args.showrejectoption,
       showfooter: args.showfooter,
       invitationmessage: dropHTMLWrapperFromMessage(args.invitationmessage),
       confirmationmessage: dropHTMLWrapperFromMessage(args.confirmationmessage),
       timezone: args.timezone,
       saved: args.saved,
       ready: true
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
    removeTypeSetters: function() {
        var document = this;
        _.each(document.signatories(), function(sig) {
            _.each(sig.fields(), function(field) {
                _.each(field.placements(), function(placement) {
                    placement.cleanTypeSetter();
                    if(placement.view)
                        placement.view.clear();
                });
            });
        });
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
            return s.elegAuthentication();
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
    }
});

});
