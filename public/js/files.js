/* Basic modules for daling with a files
 * File model + view of the files on all pages with ajax download of pages
 * Also support for showing fields placements | Sign view only for now
 */


(function(window){

window.File = Backbone.Model.extend({
    defaults: {
        id : 0,
        pages : [],
        name: "",
        broken : false
    },
    queryPart: function () {
      var params = { documentid: this.documentid(),
                     attachmentid: this.attachmentid(),
                     signatorylinkid: this.signatoryid()
                   };
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
        this.url = "/filepages/" + args.id + this.queryPart();
    },
    downloadLink : function() {
        var link = null;
        /* FIXME: this is wrong that we add .pdf as default extension. File should just know better. */
        var name = this.name();
        if (name.toLowerCase().indexOf(".pdf", name.length - 4) == -1)
          name = name + ".pdf";
        if( this.fileid()!==undefined && this.attachmentid()!==undefined ) {
            link = "/a/download/" + this.attachmentid() + "/" + this.fileid() + "/" + encodeURIComponent(name) + this.queryPart();
        }
        if( this.fileid()!==undefined && this.documentid()!==undefined ) {
            link = "/api/frontend/downloadfile/" + this.documentid() + "/" + this.fileid() + "/" + encodeURIComponent(name) + this.queryPart();
        }
        else if( this.documentid()!==undefined ) {
            link = "/api/frontend/downloadmainfile/"+ this.documentid() + "/" + encodeURIComponent(name) + this.queryPart();
        }
        else {
            console.log("File with neither documentid nor fileid, do not know where does it link to");
        }
        return link;
    },
    downloadLinkForMainFile : function(name) {
        var link = null;
        if( this.documentid()!==undefined ) {
            link = "/api/frontend/downloadmainfile/"+ this.documentid() + "/" + encodeURIComponent((name == undefined ? this.name() : name)) + ".pdf" + this.queryPart();
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
      if (this.document() != undefined && this.document().viewer().signatoryid() != undefined)
        return this.document().viewer().signatoryid();
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
                  {data: { signatoryid: this.signatoryid()},
                   processData:  true,
                   cache : false});
        }
        else
        {
            var file = this;
            var pages = _.map(response.pages, function(page,i) {
                return new FilePage({number : i + 1, file: file, width: page.width, height: page.height});
            });
            this.set({pages: pages});
        }
    }
});

/*
    File has some id, knows what document it belongs and what pages does it has

*/

var FilePage = Backbone.Model.extend({
    defaults: function() { return {
        number : 0,
        placements : [],
        width: 943,
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

var FilePageView = Backbone.View.extend({
    model : FilePage,
    initialize: function (args) {
        _.bindAll(this, 'render', 'renderDragables', 'updateDragablesPosition');
        this.model.bind('change:dragables', this.renderDragables);
        this.render();
    },
    destroy : function() {
      _.each(this.model.placements(), function(p) {
          if (p.typeSetter != undefined)
            p.typeSetter.clear();
      });
      this.off();
      this.model.off();
      $(this.el).remove();
    },
    makeDropable : function() {
      var self = this;
      var page = self.model;

      $(self.el).droppable({
          drop: function(event, ui) {
              var helper = $(ui.helper);
              /*
               * Here we need to account for border property. There is an
               * unsupported bug/feature in jquery:
               *
               * http://bugs.jquery.com/ticket/7948
               */
              var top = helper.offset().top - $(self.el).offset().top - 1;
              var left = helper.offset().left - $(self.el).offset().left - 1;
              var height = $(self.el).height();
              var width = $(self.el).width();
              var onDrop = $(ui.draggable).draggable( "option", "onDrop" );
              onDrop(page,left,top,width,height);
              return false;
          }
      });
    },
    renderDragables : function() {
        var view = this;
        var page = this.model;
        var container = $(this.el);
        var file = page.file();
        _.each(page.placements(), function(placement) {
            var placement = placement;
            if (!placement.placed() && placement.page()==page.number()) {
                var elem = $("<div>").appendTo(container);
                createFieldPlacementPlacedView({model: placement, el: elem});
            }
        });
    },
    updateDragablesPosition : function() {
        var page = this.model;
        _.each(page.placements(), function(placement) {
            if (placement.placed() && placement.page()==page.number() && placement.view != undefined && placement.view.updatePosition != undefined) {
               placement.view.updatePosition();
            }
        });
    },
    ready : function() {
        return this.pagejpg != undefined && this.pagejpg[0].complete;
    },
    render: function () {
        var page = this.model;
        var file = page.file();
        var fileid = file.fileid();

        var container = $(this.el);
        container.empty();
        container.attr("id", "page" + page.number());
        container.addClass("pagediv");

        // Page part with image
        this.pagejpg = $("<img class='pagejpg'/>");
        var pagelink = "/pages/" + fileid  + "/" + page.number() + file.queryPart();

        this.pagejpg.attr("src", pagelink);
        container.append(this.pagejpg);
        this.makeDropable();
        // Fields for the page
        this.renderDragables();
        return this;
    }
});

var FileView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.model.view = this;
        this.pageviews = [];
        this.model.fetch({data: { signatoryid: this.model.signatoryid()},
                          processData:  true,
                          cache : false});
        this.render();
    },
    destroy : function() {
      this.off();
      _.each(this.pageviews, function(pv) {pv.destroy();});
      $(this.el).remove();
    },
    vline: function() {
      if (this.vlinediv != undefined)
        return this.vlinediv;
      this.vlinediv = $("<div class='vline'>");
      $(this.el).append(this.vlinediv);
      return this.vlinediv;
    },
    hline: function() {
      if (this.hlinediv != undefined)
        return this.hlinediv;
      this.hlinediv = $("<div class='hline'>");
      $(this.el).append(this.hlinediv);
      return this.hlinediv;
    },
    render: function () {
        var view = this;
        var file = this.model;
        var docbox = $(this.el);
        docbox.empty();
        if (!file.ready()) {
            var waitbox = $("<div class='waiting4page'/>");
            docbox.append(waitbox);
        } else {
            _.each(this.pageviews, function(p) {p.destroy(); });
            this.pageviews = [];
            _.each(file.pages(),function(page){
                 var pageview = new FilePageView({model : page, el: $("<div/>")});
                 view.pageviews.push(pageview);
                 docbox.append($(pageview.el));
            });
            view.startReadyCheckerFirstPage();
            view.startReadyChecker();

        }
        return this;

    },
    startReadyChecker : function() {
        var view = this;
        if (view.ready()) {
         view.model.trigger('ready');
         if (view.pageviews != undefined)
           _.each(view.pageviews, function(pv) {
              pv.updateDragablesPosition();
           });
        }
        else
         setTimeout(function() {view.startReadyChecker()},1000);
    },
    startReadyCheckerFirstPage : function() {
        var view = this;
        if (view.readyFirstPage())
         view.model.trigger('FirstPageReady');
        else
         setTimeout(function() {view.startReadyCheckerFirstPage()},1000);
    },
    ready : function() {
        return this.model.ready() && (this.model.pages().length > 0) && (this.pageviews.length == this.model.pages().length) && _.all(this.pageviews, function(pv) {return pv.ready();});
    },
    readyFirstPage : function () {
        return this.pageviews.length > 0 && this.pageviews[0].ready();
    },
    moveCoordinateAxes : function(helper,verticaloffset) {
      var self = this;
      _.defer(function() {

        var top = helper.offset().top - $(self.el).offset().top + helper.outerHeight() + verticaloffset;
        var left = helper.offset().left - $(self.el).offset().left;
        var height = $(self.el).height();
        var width = $(self.el).width();
        self.hline().css({ top: top + "px",
                           width: width + "px"});
        self.vline().css({ left: left + "px",
                           height: height + "px"});
      });
    },
    showCoordinateAxes : function(helper,verticaloffset) {
        var view = this;
        this.hline().show();
        this.vline().show();
        this.moveCoordinateAxes(helper,verticaloffset);
    },

    hideCoordinateAxes : function() {
        this.vline().hide();
        this.hline().hide();
    }
});


window.KontraFile = {
    init : function(args){
        if (args.file != undefined) {
            this.model = args.file;
        }
        else {
            this.model = new File({
                id: args.id,
                name: args.name,
                documentid: args.documentid,
                attachmentid: args.attachmentid,
                signatoryid: args.signatoryid
            });
        }
        this.view = new FileView({
            model: this.model,
            addFields: args.addFields || false,
            el : $("<div class='document-pages'/>")
        });
        this.destroy = function() {
            this.view.destroy();
        };
        return this;
    }
};

})(window);
