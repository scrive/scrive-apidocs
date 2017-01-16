var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");
var Submit = require("./submits.js").Submit;

/* Basic modules for daling with a files
 * File model + view of the files on all pages with ajax download of pages
 * Also support for showing fields placements | Sign view only for now
 */


var File = exports.File = Backbone.Model.extend({
    defaults: function() {
      return {
        id : 0,
        pages : [],
        name: "",
        pixelWidth: 950,
        broken : false
      };
    },
    queryPart: function (more) {
      more = more || {};
      var params = _.extend(more, { document_id: this.documentid(),
                                    attachment_id: this.attachmentid(),
                                    signatory_id: this.signatoryid()
                                  });
      /*
       * Remove undefined values that may happen in the object.
       */
      _.each(_.keys(params), function (k) {
        if( params[k]===undefined ) {
          delete params[k];
        }
      });

      var query = $.param(params, true);
      if( query!="" ) {
        return "?" + query;
      }
      else {
        return "";
      }
    },
    initialize: function (args) {
        var pixelWidth = this.pixelWidth();
        this.url = "/filepages/" + args.id + this.queryPart();
    },
    downloadLink : function(as_download) {
        var link = null;
        var name = this.name();
        if( this.fileid()!==undefined && this.attachmentid()!==undefined ) {
            link = "/a/download/" + this.attachmentid() + "/" + this.fileid() + "/" + encodeURIComponent(name) + this.queryPart();
        }
        if( this.fileid()!==undefined && this.documentid()!==undefined ) {
            link = "/api/frontend/documents/" + this.documentid() + "/files/" + this.fileid() + "/" + encodeURIComponent(name) + this.queryPart({as_download : as_download});
        }
        else if( this.documentid()!==undefined ) {
            link = "/api/frontend/documents/"+ this.documentid() + "/files/main/" + encodeURIComponent(name) + this.queryPart({as_download : as_download});
        }
        else {
            console.log("File with neither documentid nor fileid, do not know where does it link to");
        }
        return link;
    },
    downloadLinkForMainFile : function(name,as_download) {
        var link = null;
        if( this.documentid()!==undefined ) {
            link = "/api/frontend/documents/"+ this.documentid() + "/files/main/" + encodeURIComponent(name + ".pdf") + this.queryPart({as_download : as_download});
        }
        else {
            console.log("File is not binded to document");
        }
        return link;
    },
    fileid : function(){
        return this.get("id");
    },
    pages : function(){
        return this.get("pages");
    },
    pixelWidth: function () {
      return this.get("pixelWidth");
    },
    placements : function() {
        return _.flatten(_.map(this.pages(), function(p) {return p.placements();}));
    },
    page : function(number){
        return this.pages()[number - 1];
    },
    document : function() {
        return this.get("document");
    },
    documentid : function(){
      if (this.document() != undefined)
        return this.document().documentid();
      return this.get("documentid");
    },
    attachmentid : function(){
        return this.get("attachmentid");
    },
    signatoryid : function(){
      if (this.document() != undefined && this.document().currentSignatory() != undefined)
        return this.document().currentSignatory().signatoryid();
      return this.get("signatoryid");
    },
    name : function(){
        return this.get("name");
    },
    ready : function(){
        return this.pages().length > 0 ;
    },
    broken : function(){
        return this.get("broken");
    },

    draftData : function() {
      return {id : this.fileid(), name: this.name()};
    },
    parse : function(response)
    {
        if (response.error != undefined)
        {
            this.set({broken: true});
        }
        else if (response.wait != undefined)
        {
          _.delay(_.bind(this.fetch, this), 2000,
                  {data: { signatory_id: this.signatoryid()},
                   processData:  true,
                   cache : false});
        }
        else
        {
            var file = this;
            var pages = [];
            for(var i=0;i<response.pages;i++)
              pages[i] = new FilePage({number : i + 1, file: file});
            this.set({pages: pages});
            if (this.ready()) {
              this.trigger("ready");
            }
        }
    }
});

/*
    File has some id, knows what document it belongs and what pages does it has

*/

var FilePage = exports.FilePage = Backbone.Model.extend({
    defaults: function() { return {
        number : 0,
        placements : [],
        width: 950,
        height: 1335
    }},
    initialize: function (args) {
    },
    file : function(){
        return this.get("file");
    },
    number : function(){
        return this.get("number");
    },
    placements : function(){
        return this.get("placements");
    },
    width: function(){
        return this.get("width");
    },
    height: function(){
        return this.get("height");
    },
    setSize : function(w,h) {
        this.set({width:w,height:h});
    },
    addPlacement : function(placement){
        var newplacements = this.placements();
        newplacements.push(placement);
        this.set({placements: newplacements});
        this.trigger("change:dragables");
    },
    removePlacement : function(placement) {
       var newplacements = new Array();
       for(var i=0;i<this.placements().length;i++)
          if (placement !== this.placements()[i])
             newplacements.push(this.placements()[i]);
       this.set({placements : newplacements}, {silent : true});
       this.trigger("change:dragables");
    }
});

