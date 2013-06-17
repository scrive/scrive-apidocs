/* Document model
 * Also document viewer (person thet is looking at document so we can hold current signatory link id somewere)
 */


(function(window) {

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

window.DocumentAuthor = Backbone.Model.extend({
   defaults: {
       fullname: "",
       email: "",
       company: "",
       phone: "",
       position: ""
    },
    fullname: function() {
        return this.get("fullname");
    },
    email: function() {
        return this.get("email");
    },
    company: function() {
        return this.get("company");
    },
    phone: function() {
        return this.get("phone");
    },
    position: function() {
        return this.get("position");
    }
});

window.Document = Backbone.Model.extend({
    defaults: {
        id: 0,
        title: "",
        signatories: [],
        file: null,
        sealedfile: null,
        authorattachments: [],
        evidenceattachments: [],
        signatoryattachments: [],
        ready: false,
        readOnlyView: false,
        viewer: new DocumentViewer(),
        infotext: "",
        authentication: "standard",
        delivery: "email",
        template: false,
        saveQueue : new AjaxQueue(),
        screenshots : {},
        flux: false
    },
    initialize: function(args) {
        var params = { evidenceAttachments: args.evidenceAttachments };
        this.url = "/api/frontend/get/" + args.id + "?" + $.param(params,true);
        _.bindAll(this);
        this.bindBubble();
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
    addExistingSignatory: function(sig) {
        var document = this;
        var signatories = document.signatories();
        signatories[signatories.length] = sig;
        var time = new Date().getTime();
        document.trigger('change:signatories');
        document.trigger('change:signorder');
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
        this.set({"authorattachments": _.without(this.authorattachments(),[a]) });
    },
    evidenceattachments: function() {
        return this.get("evidenceattachments");
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
    process: function() {
        return this.get("process");
    },
    lang: function() {
        return this.get("lang");
    },
    title: function() {
        return this.get("title");
    },
    setTitle: function(title) {
        this.set({title: title}, {silent: true});
    },
    daystosign: function() {
          return this.get("daystosign");
    },
    setDaystosign: function(daystosign) {
         this.set({"daystosign": daystosign}, {silent: true});
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
              url: "/restart/" + this.documentid(),
              method: "POST"
          });
    },
    prolong: function() {
          return new Submit({
              url: "/prolong/" + this.documentid(),
              method: "POST"
          });
    },
    canbecanceled: function() {
        return this.get("canbecanceled");
    },
    cancel: function() {
          return new Submit({
              url: "/d/cancel",
              doccheck : this.documentid(),
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
        this.set({invitationmessage: $(customtext).text() != "" ? customtext : ""},{silent: true});

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
    takeFirstScreenshot: function() {
        var document = this;
        if (document.file() && document.file().view.readyFirstPage())
            document.takeScreenshot(true, null);
        else
            document.file().bind('FirstPageReady', function() {
            document.takeScreenshot(true, null);
            });
    },
    takeSigningScreenshot: function(done) {
        this.takeScreenshot(false, done);
    },
    takeScreenshot: function(first, done) {
        var document = this;
        function callDone() {
            if (done) done();
        }
        window.takeScreenshot(
            function(canvas) {
                var shot = [ new Date().toISOString(), canvas.toDataURL("image/jpeg",0.7) ];
                if (first)
                    document.get("screenshots").first = shot;
                else
                    document.get("screenshots").signing = shot;
                callDone();
            },
            function(e) { callDone() },
            function() { callDone() },
            3000);
    },
    sign: function() {
        var document = this;
        var fields = [];
        _.each(this.currentSignatory().fields(), function(field) {
            if (field.isClosed()) return;
            fields.push({name: field.name(), value: field.value(), type: field.type()});
        });
        return new Submit({
            url : "/api/frontend/sign/" + document.documentid() +  "/" + document.currentSignatory().signatoryid(),
            method: "POST",
            screenshots: JSON.stringify(document.get("screenshots")),
            fields: JSON.stringify(fields),
            ajax: true,
            ajaxsuccess : function() {
              if (document.currentSignatory().signsuccessredirect() != undefined && document.currentSignatory().signsuccessredirect() != "") {
                window.location = document.currentSignatory().signsuccessredirect();
              } else {
                window.scroll(0,0);
                window.location.reload();
              }
            },
            ajaxerror : function() {
              window.location.reload();
            }
        });
    },
    sendByAuthor: function() {
        return new Submit({
              url : "/d/" + this.documentid(),
              timezone: jstz.determine().name(),
              send: "YES",
              method: "POST",
              ajaxtimeout : 120000
          });
    },
    signByAuthor: function() {
        var document = this;
        return new Submit({
              url : "/d/" + this.documentid(),
              timezone: jstz.determine().name(),
              sign: "YES",
              method: "POST",
              screenshots: JSON.stringify(document.get("screenshots")),
              ajaxtimeout : 120000
          });
    },
    clone : function(callback) {
       var document = this;
         return new Submit({
              url : "/api/frontend/clone/" + this.documentid(),
              method: "POST",
              ajaxtimeout : 120000,
              ajaxsuccess : function(resp) {
                var jresp = JSON.parse(resp)
                var nd = new Document({});
                nd.parse(jresp);
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
    setAttachments: function() {
        return new Submit({
              url: "/setattachments/" + this.documentid(),
              method: "POST",
              ajax: true
          });
    },
    draftData: function() {
      return {
          title: this.title(),
          invitationmessage: this.get("invitationmessage"),
          daystosign: this.get("daystosign"),
          apicallbackurl : this.get("apicallbackurl"),
          signatories: _.map(this.signatories(), function(sig) {return sig.draftData()}),
          lang: this.lang().draftData(),
          process : this.process().process(),
          template: this.isTemplate(),
          authorattachments : _.map(this.authorattachments(), function(a) {return a.draftData()})
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
       this.fixSignorder();
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
    datamismatch: function() {
        return _.any(this.signatory, function() {return this.datamismatch() == true;});

    },
    timeouttime: function() {
        return this.get("timeouttime");
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
    recall: function(successCallback) {
        console.log('recall');
        var self = this;
        var fetchOptions = { data: self.viewer().forFetch(),
                             processData: true,
                             cache: false };
        var fetchFunction = function () {
            self.fetch(fetchOptions);
        };
        fetchOptions.error = function() {
            console.error("Failed to fetch document #" + self.documentid() + ", trying again in one sec...");
            window.setTimeout(fetchFunction, 1000);
        };

        if( successCallback!=undefined ) {
            fetchOptions.success = successCallback;
        }
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
        var aidx = this.author().signorder();
        return ! _.any(this.signatories(), function(sig) {
            return sig.signs() && sig.signorder() < aidx;
            });

    },
    authorCanSignLast: function() {
        for (var i = 0; i < this.signatories().length; ++i) {
            var sig = this.signatories()[i];
            if (sig.author()) {
                if (!sig.signs() || sig.hasSigned())
                    return false;
            }
            else {
                if (sig.signs() && !sig.hasSigned())
                    return false;
            }
        }
        return true;
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
        return signatory != undefined && this.signingInProcess() && signatory.hasSigned() && !this.closed();
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
    signviewlogo: function() {
      return this.get('signviewlogo');
    },
    signviewtextcolour: function() {
      return this.get('signviewtextcolour');
    },
    signviewtextfont: function() {
      return this.get('signviewtextfont');
    },
    signviewbarscolour: function() {
      return this.get('signviewbarscolour');
    },
    signviewbarstextcolour: function() {
      return this.get('signviewbarstextcolour');
    },
    signviewbackgroundcolour: function() {
      return this.get('signviewbackgroundcolour');
    },
    authoruser: function() {
        return this.get("authoruser");
    },
    signatoriesWhoSign: function() {
        return _.filter(this.signatories(), function(sig) {
            return sig.signs();
        });
    },
    maxPossibleSignOrder : function() {
      var mpso = 0;
      _.each(this.signatories(), function(sig) {if (sig.signs()) mpso++;});
      return mpso == 0 ? 1 : mpso;
    },
    fixSignorder : function() {
        var mpso = this.maxPossibleSignOrder();
        _.each(this.signatories(), function(sig) {
          if (sig.signorder() > mpso) sig.setSignOrder(mpso);
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
     return {
       id: args.id,
       title: args.title,
       file: function() {
           if (args.file) {
               var file = new File(_.defaults(args.file, dataForFile));
               file.bind('ready', function() {
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
       evidenceattachments: args.evidenceattachments,
       signatories: _.map(args.signatories || [], function(signatoryargs) {
         return new Signatory(_.defaults(signatoryargs, { document: self }));
       }),
       authoruser: args.author != undefined ? new DocumentAuthor(_.defaults(args.author, { document: self })) : undefined,
       process: function() {
                        var process= new Process({process : args.process});
                        process.bind("change", function() { self.trigger("change:process")});
                        // This is not used but nice to have. Please leave it.
                        // It is used now --Eric
                        return process;
                }(),
       lang: (function() {
           var lang = new Lang({lang : args.lang});
           lang.bind('change', function() {
               self.trigger('change:lang');
           });
           return lang;
       }()),
       infotext: args.infotext,
       canberestarted: args.canberestarted,
       canbeprolonged: args.canbeprolonged,
       canbecanceled: args.canbecanceled,
       canseeallattachments: args.canseeallattachments,
       status: args.status,
       timeouttime: args.timeouttime == undefined ? undefined : new Date(Date.parse(args.timeouttime)),
       signorder: args.signorder,
       authentication: args.authentication,
       delivery: args.delivery,
       template: args.template,
       daystosign: args.daystosign,
       invitationmessage: args.invitationmessage,
       signviewlogo: args.signviewlogo,
       signviewtextcolour: args.signviewtextcolour,
       signviewtextfont: args.signviewtextfont,
       signviewbarscolour: args.signviewbarscolour,
       signviewbarstextcolour: args.signviewbarstextcolour,
       signviewbackgroundcolour: args.signviewbackgroundcolour,
       flux: false,
       ready: true
     };
    },
    setFlux: function() {
        this.set({flux:true});
        return this;
    },
    flux: function() {
        return this.get('flux');
    },
    unsetFlux: function() {
        this.set({flux:false});
        return this;
    },
    // validation
    hasProblems: function(forSigning) {
        return this.hasDocumentProblems() || this.hasSignatoryProblems(forSigning);
    },
    hasDocumentProblems: function() {
        return !this.hasAtLeastOneSignatory() ||
            !this.mainfile();
    },
    hasAtLeastOneSignatory: function() {
        var signing = this.signatoriesWhoSign();
        return signing.length >= 1;
    },
    hasSignatoryProblems: function(forSigning) {
        var sigs = this.signatories();
        return _.some(sigs, function(sig) {
            return sig.hasProblems(forSigning);
        });
    },
    newCheckboxName: function() {
        var document = this;
        var allnames = [];
        _.each(document.signatories(), function(s) {
            _.each(s.fields(), function(f) {
                allnames.push(f.name());
            });
        });
        var i = 1;
        while(_.contains(allnames, 'checkbox-' + i))
            i++;
        return 'checkbox-' + i;
    },
    newSignatureName: function() {
        var document = this;
        var allnames = [];
        _.each(document.signatories(), function(s) {
            _.each(s.fields(), function(f) {
                allnames.push(f.name());
            });
        });
        var i = 1;
        while(_.contains(allnames, 'signature-' + i))
            i++;
        return 'signature-' + i;
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
    bindBubble: function() {
        var document = this;
        document.bind('change', document.bubbleSelf);
    },
    bubbleSelf: function() {
        var document = this;
        document.trigger('bubble');
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
      })
    },
    csv : function() {
      var csv = undefined;
      _.each(this.signatories(),function(s) {
        csv = csv || s.csv();
      });
      return csv;
    },
    normalizeWithFirstCSVLine : function() {
      return _.each(this.signatories(),function(s) {
          if (s.isCsv())
            s.normalizeWithFirstCSVLine();
      })
    },
    dropFirstCSVLine : function() {
      return _.each(this.signatories(),function(s) {
          if (s.isCsv())
            s.dropFirstCSVLine();
      })
    }
});


// This is an object that allows you to fill a dom element
window.DocumentDataFiller = {
    fill: function(document, object) {
        // Filling title
        var title = document.title();
        $(".documenttitle", object).text(title);

        // Filling unsigned signatories
        var unsignedpartynotcurrent = [];
        var unsignedparty = [];

        var signatories = _.select(document.signatories(), function(signatory) {
            return signatory.signs() && !signatory.hasSigned();
        });

        for (var i = 0; i < signatories.length; i++)
            if (signatories[i].current())
            {
                unsignedparty.push(localization.you);
            }
            else if (signatories[i].isCsv()) {
                unsignedparty.push(localization.csvFilePersons);
                unsignedpartynotcurrent.push(localization.csvFilePersons);
            }
            else
            {
                unsignedparty.push(signatories[i].smartname());
                unsignedpartynotcurrent.push(signatories[i].smartname());
            }

        var escapeHTML = function(s) {
            var result = '';
            for (var i = 0; i < s.length;++i) {
                var c = s.charAt(i);
                if (c == '&')
                    result += '&amp';
                else if (c == '\'')
                    result += '&#39;';
                else if (c == '"')
                    result += '&quot;';
                else if (c == '<')
                    result += '&lt;';
                else if (c == '>')
                    result += '&gt;';
                else
                    result += c;
            }
            return result;
        };
        var listStringMany = function(names) {
            var name0 = names.shift();
            if (names.length === 1)
                return "<strong>" + escapeHTML(name0) + "</strong> " + localization.listand + " <strong>" + escapeHTML(names[0]) + "</strong>";
            return "<strong>" + escapeHTML(name0) + "</strong>, " + listStringMany(names);
        };
        var listString = function(names) {
            if (names.length === 0)
                return "";
            if (names.length === 1)
                return "<strong>" + escapeHTML(names[0]) + "</strong>";
            if (names.length === 2)
                return "<strong>" + escapeHTML(names[0]) + "</strong> " + localization.and + " <strong>" + escapeHTML(names[1]) + "</strong>";
            return listStringMany(names);
        };
        $(".unsignedpart", object).html(listString(unsignedparty));
        $(".unsignedpartynotcurrent", object).html(listString(unsignedpartynotcurrent));
        return object;
        // Something more can come up
    }
};

})(window);
