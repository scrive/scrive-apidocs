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
      /* This construct removes undefined values from params */
      params = _.defaults({}, params);

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
        if( this.fileid()!==undefined ) {
            link = "/download/" + this.fileid() + "/" + this.name() + ".pdf" + this.queryPart();
        }
        else if( this.documentid()!==undefined ) {
            link = "/downloadmainfile/"+ this.documentid() + "/" + this.name() + ".pdf" + this.queryPart();
        }
        else {
            console.log("File with neither documentid nor fileid, do not know where does it link to");
        }
        return link;
    },
    downloadLinkForMainFile : function() {
        var link = null;
        if( this.documentid()!==undefined ) {
            link = "/downloadmainfile/"+ this.documentid() + "/" + this.name() + ".pdf" + this.queryPart();
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
        return this;
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
    },
    fixedX : function(x,y,field){
     /* Turn off snap-to-grid for now. It is nice when it works, but
        it does not always do what you want. -- Eric
     _.each(this.placements(), function(p) {
             if (Math.abs(p.x()-x) < 8 && (Math.abs(p.x()-x) + Math.abs(p.y()-y) < 400))
                  x = p.x();
     });
     */
     return x;
    },
    fixedY : function(x,y,field){
    /* Turn off snap-to-grid for now. It is nice when it works, but
       it does not always do what you want. -- Eric
          _.each(this.placements(), function(p) {
             if (Math.abs(p.y()-y) < 8 && (Math.abs(p.x()-x) + Math.abs(p.y()-y) < 400))
                  y =  p.y();
     });
     */
     return y;
    }
});

var FilePageView = Backbone.View.extend({
    model : FilePage,
    initialize: function (args) {
        _.bindAll(this, 'render', 'renderDragables');
        this.model.bind('change:dragables', this.renderDragables);
        this.model.view = this;
        this.renderedPlacements = [];
        this.render();
    },
    vline: function() {
        if (this.vlinediv != undefined)
            return this.vlinediv;
        this.vlinediv = $("<div class='vline'>");
        this.pagejpg.append(this.vlinediv);
        return this.vlinediv;
    },
    hline: function() {
        if (this.hlinediv != undefined)
            return this.hlinediv;
        this.hlinediv = $("<div class='hline'>");;
        this.pagejpg.append(this.hlinediv);
        return this.hlinediv;
    },
    moveCoordinateAxes : function(helper) {
       this.hline().css({
                top: Math.min(this.pagejpg.height() - 1, Math.max(0, helper.offset().top - this.pagejpg.offset().top + helper.height() - 4)) + "px"
            });


       this.vline().css({
                left: Math.min(this.pagejpg.width() - 1, Math.max(0, helper.offset().left - this.pagejpg.offset().left)) + "px"
            });
    },
    showCoordinateAxes : function(helper) {
        var view = this;
        this.hline().show();
        this.vline().show();
        $("body").mousemove(function() {
            setTimeout(function() {
                view.moveCoordinateAxes(helper);
            }, 100);
        });
    },

    hideCoordinateAxes : function() {
        this.vline().hide();
        this.hline().hide();
        $("body").unbind("mousemove");
    },
    makeDropable : function() {
      var page = this.model;
      var pagejpg = this.pagejpg;
      pagejpg.droppable({
        drop: function(event, ui) {
            var helper = $(ui.helper);
            var top = helper.offset().top -  pagejpg.offset().top;
            var left = helper.offset().left -  pagejpg.offset().left;
            var options = $(ui.draggable).data("draggable").options;
            var onDrop = options.onDrop;
            onDrop(page,left,top);
            return false;
          }
       });
    },
    renderDragables : function() {
        var view = this;
        var page = this.model;
        var container = $(this.el);
        var file = page.file();

        this.renderedPlacements = [];
        $(".placedfield",container).detach();
        _.each(page.placements(), function(placement) {
            var placement = placement;
            if (placement.page()==page.number()) {
                var elem = new FieldPlacementPlacedView({model: placement, el : $("<div>")}).el;
                container.append(elem);
                if (!placement.field().isClosed()) {
                  view.renderedPlacements.push({
                    placement: placement,
                    elem: $(elem)
                  });
                }
            }
        });
    },
    ready : function() {
        return this.pagejpg != undefined && this.pagejpg.width() > 100 && this.pagejpg.height() > 100;
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
        this.pagejpg = $("<div class='pagejpg'/>");
        var pagelink = location.protocol + "//" + location.host + "/pages/" + fileid  + "/" + page.number() + file.queryPart();

        this.pagejpg.css("background-image", "url(" +pagelink +")");
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
    render: function () {
        var view = this;
        var file = this.model;
        var docbox = $(this.el);
        docbox.empty();
        if (!file.ready()) {
            var waitbox = $("<div class='waiting4page'/>");
            docbox.append(waitbox);
        } else {
            this.pageviews = [];
            _.each(file.pages(),function(page){
                 var pageview = new FilePageView({model : page, el: $("<div/>")});
                 view.pageviews.push(pageview);
                 docbox.append($(pageview.el));
            });
            view.startReadyChecker();

        }
        return this;

    },
    startReadyChecker : function() {
        var view = this;
        if (view.ready())
         view.model.trigger('ready');
        else
         setTimeout(function() {view.startReadyChecker()},1000);
    },
    ready : function() {
        //console.log("Document view checking ...")
        //console.log("File is ready " + this.model.ready())
        //console.log("Pages length "  +  (this.pageviews.length) + " " + (this.model.pages().length))
        //console.log("All pages ready "  +  _.all(this.pageviews, function(pv) {return pv.ready();}))
        return this.model.ready() && (this.model.pages().length > 0) && (this.pageviews.length == this.model.pages().length) && _.all(this.pageviews, function(pv) {return pv.ready();});
    },
    moveCoordinateAxes : function(helper) {
        var file = this.model;
        _.each(file.pages(),function(page){
                 page.view.moveCoordinateAxes(helper);
            });
    },

    showCoordinateAxes : function(helper) {
        var file = this.model;
        _.each(file.pages(),function(page){
                 page.view.showCoordinateAxes(helper);
            });
    },

    hideCoordinateAxes : function() {
        var file = this.model;
        _.each(file.pages(),function(page){
                 page.view.hideCoordinateAxes();
            });
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
            el : $("<div id ='documentBox'/>")
        });
        return this;
    }
};

})(window);
