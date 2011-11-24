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
              pages[i] = new FilePage({number : i + 1, file: this, width: response.pages[i].width, height: response.pages[i].height})
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
        this.trigger("change");
    }
});

var FilePageView = Backbone.View.extend({
    model : FilePage,
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    render: function () {
        var model = this.model;
        var container = this.el;
        container.empty();
        container.attr("id", "page" + model.number());
        container.addClass("pagediv");
        
        // Page part with image
        var pagejpg = $("<div class='pagejpg'/>")
        var pagelink = "/pages/" + model.file().document().documentid() + "/" + model.file().fileid()  + "/" + model.number() + model.file().document().viewer().urlPart() ;
        pagejpg.css("background-image", "url(" +pagelink +")");
        pagejpg.append($("<input type='hidden' name='width'/>").val(model.width()));
        pagejpg.append($("<input type='hidden' name='height'/>").val(model.height()));
        container.append(pagejpg);
        
        // Fields for the page
        _.each(model.placements(), function(placement) {
            var val = $("<span class='value'/>");
            var field =  placement.field();
            if (placement.field().value() != "")
                    val.text(field.value());
                else
                    val.text(field.nicename());
            field.bind('change', function() {
                if (placement.field().value() != "")
                    val.text(field.value());
                else
                    val.text(field.nicename());
            });
            var place = $("<div style='position: absolute;' class='placedfield'>");
            place.offset({
                left: placement.x(),
                top: placement.y()
            });
            place.append(val);
            container.append(place);
        })
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
        var docbox = this.el
        docbox.attr("id","documentBox");
        docbox.empty();
        if (!file.ready()) {
            var waitbox = $("<div class='waiting4page'/>")
            docbox.append(waitbox);
        } else {
            _.each(file.pages(),function(page){
                 var pageview = new FilePageView({model : page, el: $("<div/>")});
                 docbox.append(pageview.el);
            })
        }
        return this;

    }
});
  

window.KontraFile = {
    init : function(args){
        if (args.file != undefined)
           this.model = args.file
        else
            this.model = new File({
                        id : args.id,
                        name : args.name,
                        document : args.document
                    })
        this.view = new FileView({
                        model: this.model,
                        el : $("<div/>")
                    })
        return this;
    }
}

})(window); 
