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
    initialize: function (args) {
        this.url = "/filepages/" + args.document.documentid() + "/" + args.id;
    },
    downloadLink : function() {
        return "/download/"+ this.document().documentid() + "/" + this.fileid() +"/"+ this.name() + ".pdf" + this.document().viewer().urlPart();
    },
    fileid : function(){
        return this.get("id");
    },
    pages : function(){
        return this.get("pages");
    },
    page : function(number){
        var pages = this.pages();
        for(var i=0;i<pages.length;i++)
            if (pages[i].number() == number)
                return pages[i];

    },
    document : function(){
        return this.get("document");
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
        var document = this.document();
        if (response.error != undefined)
        { this.set({broken: true});
          return this;
        }
        else if (response.wait != undefined)
        {
          var current = this;
          setTimeout(function(){current.fetch({data: document.viewer().forFetch(),   processData:  true, cache : false});},2000);
          return this;
        }
        else {
          var pages  = new Array();
          for(var i = 0;i < response.pages.length; i++){
              pages[i] = new FilePage({number : i + 1, file: this, width: response.pages[i].width, height: response.pages[i].height});
          }
          this.set({pages: pages});
          return this;
        }


    }
});

/*
    File has some id, knows what document it belongs and what pages does it has

*/

var FilePage = Backbone.Model.extend({
    defaults: {
        number : 0,
        placements : [],
        width: 943,
        height: 1335
    },
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
        _.bindAll(this, 'render', 'renderDragables');
        this.model.bind('change:dragables', this.renderDragables);
        this.model.view = this;
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
        var document =file.document();
        $(".placedfield",container).remove();
        _.each(page.placements(), function(placement) {
            var placement = placement;
            if (placement.page()==page.number()) {
                container.append(new FieldPlacementPlacedView({model: placement, el : $("<div>")}).el);
            }
        });
    },
    render: function () {
        var page = this.model;
        var file = page.file();
        var document =file.document();
        var container = $(this.el);
        container.empty();
        container.attr("id", "page" + page.number());
        container.addClass("pagediv");

        // Page part with image
        this.pagejpg = $("<div class='pagejpg'/>");
        var pagelink = location.protocol + "//" + location.host + "/pages/" + document.documentid() + "/" + file.fileid()  + "/" + page.number() + document.viewer().urlPart() ;
        this.pagejpg.css("background-image", "url(" +pagelink +")");
        this.pagejpg.append($("<input type='hidden' name='width'/>").val(page.width()));
        this.pagejpg.append($("<input type='hidden' name='height'/>").val(page.height()));
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
        this.model.fetch({data: this.model.document().viewer().forFetch(),   processData:  true, cache : false});
        this.render();
    },
    render: function () {
        var file = this.model;
        var docbox = $(this.el);
        docbox.attr("id","documentBox");
        docbox.empty();
        if (!file.ready()) {
            var waitbox = $("<div class='waiting4page'/>");
            docbox.append(waitbox);
        } else {
            _.each(file.pages(),function(page){
                 var pageview = new FilePageView({model : page, el: $("<div/>")});
                 docbox.append($(pageview.el));
            });
        }
        return this;

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
        if (args.file != undefined)
           this.model = args.file;
        else
            this.model = new File({
                        id : args.id,
                        name : args.name,
                        document : args.document
                    });
        this.view = new FileView({
                        model: this.model,
                        el : $("<div/>")
                    });
        return this;
    }
};

})(window);
