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
        signatoryattachments: [],
        ready: false,
        readOnlyView: false,
        viewer: new DocumentViewer(),
        infotext: "",
        authentication: "standard",
        delivery: "email",
        template: false,
        saveQueue : new AjaxQueue()
    },
    initialize: function(args) {
        this.url = "/api/frontend/get/" + args.id;
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
    addSignatory: function() {
      var document = this;
      var signatories = document.signatories();
      var author = document.author();
      var signorder = 1;
      if( author.signs()) {
        if( author.signorder()==1 ) {
          // author signs first
          signorder = 2;
        }
        else {
          // author does not sign first
          signorder = 1;
        }
      }
      else {
        // author does not sign at all
        signorder = 1;
      }
      var nsig = new Signatory({"document": document, signs: true, signorder: signorder});
      signatories[signatories.length] = nsig;
      document.set({"signatories": signatories});
      this.fixSignorder();
      return nsig;
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
    restart: function() {
          return new Submit({
              url: "/restart/" + this.documentid(),
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
        this.set({invitationmessage: customtext},{silent: true});
    },
    inviteMail: function() {
                return new Mail({
                                                document: this,
                                                type: "invite",
                                                editWidth: 300
                        });
    },
    sign: function() {
        var document = this;
        var fields = [];
        _.each(this.currentSignatory().fields(), function(field) {
            if (field.isClosed()) return;
            fields.push({name: field.name(), value: field.value(), type: field.type()});
        });
        var url;
        //        if(document.preparation() || (document.viewer() && document.viewer().signatoryid() === document.author().signatoryid())) // author
        //url = "/d/" + document.documentid();
        //else
        url = "/s/" + document.documentid() +  "/" + document.viewer().signatoryid();
        return new Submit({
            sign : "YES",
            url : url,
            method: "POST",
            fields: JSON.stringify(fields)
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
        return new Submit({
              url : "/d/" + this.documentid(),
              timezone: jstz.determine().name(),
              sign: "YES",
              method: "POST",
              ajaxtimeout : 120000
          });
    },
    save: function() {
         this.get("saveQueue").add(new Submit({
              url: "/api/frontend/update/" + this.documentid(),
              method: "POST",
              json: JSON.stringify(this.draftData())
          }));
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
          authentication: this.get("authentication"),
          delivery: this.get("delivery"),
          apicallbackurl : this.get("apicallbackurl"),
          signatories: _.map(this.signatories(), function(sig) {return sig.draftData()}),
          lang: this.lang().draftData(),
          process : this.process().process(),
          template: this.isTemplate()
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
    standardAuthentication: function() {
          return this.get("authentication") == "standard";
    },
    elegAuthentication: function() {
          return this.get("authentication") == "eleg";
    },
    emailDelivery: function() {
          return this.get("delivery") == "email";
    },
    padDelivery : function() {
          return this.get("delivery") == "pad";
    },
    apiDelivery : function() {
          return this.get("delivery") == "api";
    },
    setStandardAuthentication: function() {
          this.set({"authentication": "standard"}, {silent: true});
          this.trigger("change:authenticationdelivery");
    },
    setElegAuthentication : function() {
          this.set({"authentication":"eleg"}, {silent: true});
          this.trigger("change:authenticationdelivery");
    },
    setEmailDelivery: function() {
          this.set({"delivery": "email"}, {silent: true});
          this.trigger("change:authenticationdelivery");
    },
    setPadDelivery : function() {
          this.set({"delivery":"pad"}, {silent: true});
          _.each(this.signatories(), function(sig) {sig.clearAttachments();});
          this.trigger("change:authenticationdelivery");
    },
    setAPIDelivery : function() {
          this.set({"delivery":"api"}, {silent: true});
          this.trigger("change:authenticationdelivery");
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
       return this.set({"template": true}, {silent: true});
    },
    recall: function(successCallback) {
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
      for (var i = 0; i < this.signatories().length; i++)
          if (this.signatories()[i].author())
              return this.signatories()[i];
    },
    authorCanSignFirst : function() {
        if (!this.author().signs() || this.padDelivery())
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
    logo: function() {
        return this.get("logo");
    },
    barsbackgroundcolor: function() {
        return this.get("barsbackgroundcolor");
    },
    barsbackgroundtextcolor: function() {
        return this.get("barsbackgroundtextcolor");
    },
    authoruser: function() {
        return this.get("authoruser");
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
      return this.closed() && this.mainfile() == undefined;
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
     return {
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
       signatories: _.map(args.signatories, function(signatoryargs) {
         return new Signatory(_.defaults(signatoryargs, { document: self }));
       }),
       authoruser: new DocumentAuthor(_.defaults(args.author, { document: self })),
       process: function() {
                        var process= new Process({process : args.process});
                        process.bind("change", function() { self.trigger("change:process")});
                        // This is not used but nice to have. Please leave it.
                        return process;
                }(),
       lang: new Lang({lang : args.lang}),
       infotext: args.infotext,
       canberestarted: args.canberestarted,
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
       logo: args.logo,
       barsbackgroundcolor: args.barsbackgroundcolor,
       barsbackgroundtextcolor: args.barsbackgroundtextcolor,
       ready: true
     };
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
