/* Whole document schema
 */


(function(window){
/*
 * Document has id and knows if its signed
 */


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
        authorization: "email"
        
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
        if (this.get("sealedfiles").length > 0)
            return this.get("sealedfiles")[0];
        else
            return this.get("files")[0];
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
    parse: function(args) {
     var document = this;   
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
      ready: true
      };
    }
    
});

})(window); 
