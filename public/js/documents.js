/* Document model
 * Also document viewer (person thet is looking at document so we can hold current signatory magic hash somewere)
 */


(function(window) {

window.DocumentViewer = Backbone.Model.extend({
    signatoryid: function() {
      return this.get("signatoryid");
    },
    magichash: function() {
      return this.get("magichash");
    },
    urlPart: function() {
        if (this.signatoryid() != undefined && this.magichash() != undefined)
            return "?signatorylinkid=" + this.signatoryid() + "&magichash=" + this.magichash();
        else return "";
    },

    forFetch: function() {
        return {
            signatoryid: this.signatoryid(),
            magichash: this.magichash()
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
        files: [],
        sealedfiles: [],
        authorattachments: [],
        signatoryattachments: [],
        ready: false,
        viewer: new DocumentViewer(),
        infotext: "",
        authorization: "email",
        template: false
    },
    initialize: function(args) {
        this.url = "/doc/" + args.id;
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
        if (sigs.length > 1) //We try not to show author on this list unless he is only one left due to sign-last functionality misdesign. |
            return _.filter(sigs,function(sig) {return !sig.author()});
        return sigs;
    },
    addSignatory: function() {
        var document = this;
        var signatories = this.signatories();
        var nsig = new Signatory({"document": document, signs: true});
        signatories[signatories.length] = nsig;
        document.set({"signatories": signatories});
        document.change();
        return nsig;
    },
    mainfile: function() {
        var file;
        if (this.closed())
            file = this.get("sealedfiles")[0];
        else
            file = this.get("files")[0];
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
    process: function() {
        return this.get("process");
    },
    region: function() {
        return this.get("region");
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
         this.set({daystosign: daystosign}, {silent: true});
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
              url: "/cancel/" + this.documentid(),
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
        var fieldnames = [];
        var fieldvalues = [];
        _.each(this.currentSignatory().fields(), function(field) {
            if (field.isClosed()) return;
            fieldnames.push(field.name());
            fieldvalues.push(field.value());
        });
          return new Submit({
              sign : "YES",
              url : "/s/" + this.documentid() + "/" + this.viewer().signatoryid(),
              method: "POST",
              magichash: this.viewer().magichash(),
              fieldname: fieldnames,
              fieldvalue: fieldvalues
          });
    },
    sendByAuthor: function() {
        return new Submit({
              send: "YES",
              method: "POST"
          });
    },
    signByAuthor: function() {
        return new Submit({
              sign: "YES",
              method: "POST"
          });
    },
    save: function() {
         return new Submit({
              url: "/save/" + this.documentid(),
              method: "POST",
              draft: JSON.stringify(this.draftData())
          });
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
          authorization: this.get("authorization"),
          signatories: _.map(this.signatories(), function(sig) {return sig.draftData()}),
          region: this.region().draftData(),
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
    },
    currentViewerIsAuthor: function() {
        var csig = this.currentSignatory();
        return (csig != undefined && csig.author());
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
    getFile: function(fileid)
    {
        return _.detect(this.get("files"), function(file) {
           return file.fileid() == fileid;
        });
    },
    signorder: function() {
      return this.get("signorder");
    },
    emailAuthorization: function() {
          return this.get("authorization") == "email";
    },
    elegAuthorization: function() {
          return this.get("authorization") == "eleg";
    },
    padAuthorization : function() {
          return this.get("authorization") == "pad";
    },
    setElegVerification : function() {
          this.set({"authorization":"eleg"}, {silent: true});
          this.trigger("change:authorization");
    },
    setEmailVerification: function() {
          this.set({"authorization": "email"}, {silent: true});
          this.trigger("change:authorization");
    },
    setPadVerification : function() {
          this.set({"authorization":"pad"}, {silent: true});
          _.each(this.signatories(), function(sig) {sig.clearAttachments();});
          this.trigger("change:authorization");
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
    recall: function() {
       var doc = this;
       this.fetch({data: this.viewer().forFetch(),
                   processData: true,
                   cache: false,
                   error: function() {
                    console.error("Failed to fetch doc, trying again ...");
                    window.setTimeout(doc.recall, 1000);
                  }
      });
    },
    needRecall: function() {
        return this.mainfile() == undefined;
    },
    author: function() {
      for (var i = 0; i < this.signatories().length; i++)
          if (this.signatories()[i].author())
              return this.signatories()[i];
    },
    authorCanSignFirst : function() {
        if (!this.author().signs() || this.padAuthorization())
            return false;
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
    isAuthorAttachments: function() {
      return this.authorattachments().length > 0;
    },
    isSignatoryAttachments: function() {
      return this.currentSignatory().attachments().length > 0;
    },
    isUploadedAttachments: function() {
      return this.canseeallattachments() && this.signatoryattachments().length > 0;
    },
    currentSignatoryCanSign: function() {
      var canSignAsSig = !this.currentViewerIsAuthor() &&
                              this.currentSignatory() != undefined &&
                              this.currentSignatory().canSign();
      var canSignAsAuthor = this.currentViewerIsAuthor() &&
                                 this.authorCanSignLast();
      return canSignAsSig || canSignAsAuthor;
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
    isWhiteLabeled: function() {
        return this.get("whitelabel");
    },
    parse: function(args) {
     var document = this;
     setTimeout(function() {
         if (document.needRecall())
            document.recall();
     },1000);
     var extendedWithDocument = function(hash) {
                hash.document = document;
                return hash; };
     /**this way of doing it is safe for IE7 which doesnt
      * naturally parse stuff like 2012-03-29 so new Date(datestr)
      * doesnt work*/
     var parseDate = function(datestr) {
        var dateValues = datestr.split('-');
        return new Date(dateValues[0], dateValues[1], dateValues[2]);
     };
     return {
      title: args.title,
      files: _.map(args.files, function(fileargs) {
                return new File(extendedWithDocument(fileargs));
              }),
      sealedfiles: _.map(args.sealedfiles, function(fileargs) {
                return new File(extendedWithDocument(fileargs));
              }),
      authorattachments: _.map(args.authorattachments, function(fileargs) {
                return new File(extendedWithDocument(fileargs));
              }),
      signatories: _.map(args.signatories, function(signatoryargs) {
                return new Signatory(extendedWithDocument(signatoryargs));
      }),
      authoruser: new DocumentAuthor(extendedWithDocument(args.author)),
      process: new Process(args.process),
      region: new Region(args.region),
      infotext: args.infotext,
      canberestarted: args.canberestarted,
      canbecanceled: args.canbecanceled,
      canseeallattachments: args.canseeallattachments,
      status: args.status,
      timeouttime: args.timeouttime == undefined ? undefined : parseDate(args.timeouttime),
      signorder: args.signorder,
      authorization: args.authorization,
      template: args.template,
      daystosign: args.daystosign,
      invitationmessage: args.invitationmessage,
      logo: args.logo,
      barsbackgroundcolor: args.barsbackgroundcolor,
      barsbackgroundtextcolor: args.barsbackgroundtextcolor,
      whitelabel: args.whitelabel,
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
