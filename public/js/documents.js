/* Document model 
 * Also document viewer (person thet is looking at document so we can hold current signatory magic hash somewere)
 */


(function(window){

window.DocumentViewer = Backbone.Model.extend({
    signatoryid : function(){
      return this.get("signatoryid");
    },
    magichash : function(){
      return this.get("magichash");
    },
    urlPart : function() {
        if (this.signatoryid() != undefined && this.magichash() != undefined)
            return "/" + this.signatoryid() + "/" + this.magichash();
        else return "";
    },
    forFetch : function() {
        return {
            signatoryid : this.signatoryid(),
            magichash :  this.magichash()                                             
        };
    }
});
    
window.Document = Backbone.Model.extend({
    defaults: {
        id : 0,
        title : "",
        signatories: [],
        files : [],
        sealedfiles : [],
        authorattachments : [],
        signatoryattachments : [],
        ready: false,
        viewer: new DocumentViewer(),
        infotext: "",
        authorization: "email",
        template : false
        
    },
    initialize: function (args) {
        this.url = "/doc/" + args.id;
    },
    viewer : function(){
        return this.get("viewer");
    },
    signed : function(){
        return this.get("signed");
    },
    documentid: function(){
        return this.get("id");
    },
    signatories: function(){
        return this.get("signatories");
    },
    mainfile: function(){
        var file;
        if (this.closed())
            file =  this.get("sealedfiles")[0];
        else
            file = this.get("files")[0];
        return file;
    },
    authorattachments: function(){
        return this.get("authorattachments");
    },
    signatoryattachments: function(){
        return _.flatten(_.map(this.signatories(), function(sig){ return sig.attachments(); } ));
    },
    hasAnyAttachments: function() {
        return this.authorattachments().length > 0 || this.signatoryattachments().length > 0;
    },
    ready: function(){
        return this.get("ready");
    },
    process: function(){
        return this.get("process");
    },
    title : function(){
        return this.get("title");
    },
    setTitle : function(title) {
        this.set({title: title}, {silent: true});
    },
    infotext : function(){
        return this.get("infotext");
    },
    canberestarted : function() {
        return this.get("canberestarted");
    },
    restart : function(){
          return new Submit({
              url: "/restart/" + this.documentid(),
              method: "POST"
          });
    },
    cancel : function(customtext){
          return new Submit({
              url: "/cancel/" + this.documentid(),
              method: "POST",
			  customtext : customtext
          });
    },
	cancelMail : function(){
          	return new Mail({
						document: this,
						type: "cancel"
			});
    },
    sign : function() {
        var fieldnames = [];
        var fieldvalues = [];
        _.each(this.currentSignatory().fields(),function(field){
            if (field.isClosed()) return;
            fieldnames.push(field.name());  
            fieldvalues.push(field.value());   
        });
          return new Submit({
              sign : "YES",
              method: "POST",
              fieldname : fieldnames,
              fieldvalue : fieldvalues
          });
    },
    save : function() {
         return new Submit({
              method: "POST"
          });
    },
    switchToAdvanced : function() {
          return this.save.add("changefunctionality","true").add("toadvanced","true");
    },
    switchToBasic : function() {
          return this.save.add("changefunctionality","true").add("tobasic","true");
    },
    saveAsTemplate : function() {
          return this.save().add("template", "YES");
    },
    status : function() {
          return this.get("status");  
    },
    currentSignatory : function() {
       return _.detect(this.signatories(), function(signatory) {
           return signatory.current();
        });

    },
    otherSignatories : function(){
       return _.select(this.signatories(), function(signatory) {
           return !signatory.current();
       });
    },
    currentViewerIsAuthor : function() {
        var csig  = this.currentSignatory();
        return  (csig != undefined && csig.author());
    },
    pending: function() {
        return this.status() == "Pending";
    },
    awaitingauthor: function() {
        return this.status() == "AwaitingAuthor";
    },
    timedout :function() {
        return this.status() == "Timedout";
    },
    canceled :function() {
        return this.status() == "Canceled";
    },
    closed :function() {
        return this.status() == "Closed";
    },
    signingInProcess: function(){
        return this.pending() || this.awaitingauthor();
    },
    datamismatch :function() {
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
    signorder : function() {
      return this.get("signorder");
    },
    emailAuthorization : function() {
          return this.get("authorization") == "email";
    },
    elegAuthorization : function() {
          return this.get("authorization") == "eleg";
    },
    elegTBS : function() {
        var text = this.title() + " "+  this.documentid() ;
        _.each(this.signatories(),function(signatory) {
            text += " "+signatory.name() + " " + signatory.personalnumber();
        });
        return text;
    },
    lastSignatoryLeft : function() {
        return _.all(this.signatories(), function(signatory) {
          return (signatory.signs() && signatory.hasSigned()) || !signatory.signs() || signatory.current(); 
        });
    },
    isTemplate: function() {
      return this.get("template") == true
    },  
    recall : function() {
       this.fetch({data: this.viewer().forFetch(),   processData:  true, cache : false});  
    },
    needRecall : function() {
        return this.mainfile() == undefined;
        {   var document = this;
            setTimeout(function() {
                        document.fetch({data: document.viewer().forFetch(),   processData:  true, cache : false});
                        }, 1000);
        }
    },
    parse: function(args) {
     var document = this;   
     setTimeout(function() {
         if (document.needRecall())
            document.recall();
     },1000);                                              
     var extendedWithDocument = function(hash){
                hash.document = document;
                return hash; };     
     return {
      title : args.title,
      files : _.map(args.files, function(fileargs) {
                return new File(extendedWithDocument(fileargs));
              }),
      sealedfiles : _.map(args.sealedfiles, function(fileargs) {
                return new File(extendedWithDocument(fileargs));   
              }),
      authorattachments : _.map(args.authorattachments, function(fileargs) {
                return new File(extendedWithDocument(fileargs));   
              }),
      signatories : _.map(args.signatories, function(signatoryargs){
                return new Signatory(extendedWithDocument(signatoryargs));
      }),
      process: new Process(args.process),
      infotext : args.infotext,
      canberestarted : args.canberestarted,
      status : args.status,
      timeouttime  : args.timeouttime  == undefined ? undefined :  new Date(args.timeouttime),
      signorder : args.signorder,
      authorization : args.authorization,
      template : args.template,
      ready: true
      };
    }
    
});


// This is an object that allows you to fill a dom element
window.DocumentDataFiller = {
    fill : function (document,object) {
        // Filling title
        var title = document.title();
        $(".documenttitle", object).text(title);
        
        // Filling unsigned signatories
        var unsignedpartynotcurrent = "";
        var signatories = _.select(document.signatories(), function(signatory){
            return signatory.signs() && !signatory.hasSigned() && !signatory.current();
        });
        for(var i=0;i<signatories.length;i++) {
               unsignedpartynotcurrent += signatories[i].smartname();
           if (i < signatories[i].length - 1)
               unsignedpartynotcurrent += ",";
        }
        $(".unsignedpartynotcurrent", object).text(unsignedpartynotcurrent);
        // Something more can come up
    }
}

})(window); 
