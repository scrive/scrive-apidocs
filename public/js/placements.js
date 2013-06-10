/* Placements of fields for signatories */


(function(window){

window.FieldPlacement = Backbone.Model.extend({
    defaults: {
      placed : false,
      tip: undefined,
      wrel: 0,
      hrel: 0,
      fsrel: 0.153,
      withTypeSetter : false,
      alive: true,
      step: 'edit'
    },
    initialize : function(args){
        var placement = this;
        _.bindAll(placement);
        placement.addToPage();
        if (this.tip() === undefined)
          this.set({"tip" : args.field.defaultTip()});
        this.setSignatory(args.field.signatory());
        this.setField(args.field);
    },
    placed : function() {
        return this.get("placed");
    },
    addToPage : function() {
        if (this.placed())
            return;
        var placement = this;
        var field = placement.field();
        var signatory = field.signatory();
        var document = signatory.document();
        var pageno = this.get("page");
        var tryToAddToPage = function() {
            if (placement.alive() && document.file() && document.file().page(pageno) != undefined) {
                document.file().page(pageno).addPlacement(placement);
                placement.set({placed : true});
                document.off('file:change',tryToAddToPage);
            }
            if(!placement.alive())
                document.off('file:change',tryToAddToPage);
        };
        document.bind('file:change',tryToAddToPage);
        setTimeout(tryToAddToPage,0);
    },
    xrel : function() {
        return this.get("xrel");
    },
    yrel : function() {
        return this.get("yrel");
    },
    wrel : function() {
        return this.get("wrel");
    },
    hrel : function() {
        return this.get("hrel");
    },
    fsrel : function() {
        return this.get("fsrel");
    },
    page : function() {
        return this.get("page");
    },
    field : function(){
        return this.get("field");
    },
    setField: function(f) {
        var oldfield = this.field();
        if(oldfield !== f) {
            if(f) {
                f.bind('removed', this.remove);
                f.addPlacement(this);
            }
            if(oldfield) {
                oldfield.unbind('removed', this.remove);
                oldfield.removePlacement(this);
            }
            this.set({field:f});
        }
    },
    file : function(){
        return this.get("file");
    },
    tip : function(){
        return this.get("tip");
    },
    withTypeSetter : function() {
      return this.get("withTypeSetter") == true;
    },
    cleanTypeSetter : function() {
       this.set({"withTypeSetter" : false});
        this.trigger('clean');
    },
    remove : function() {
       this.trigger("removed");
       var document = this.field().signatory().document();
       var page = document.file().page(this.get("page"));
       page.removePlacement(this);
        this.set({placed:false});
       this.field().removePlacement(this);
       this.off();
    },
    fixWHRel : function(w,h) {
      var page =  this.field().signatory().document().mainfile().page(this.get("page"));
      if (page != undefined && page.width() != undefined && page.height() != undefined)
          this.set({ wrel : (w / page.width()) , hrel : (h / page.height()) },    {silent : true});
    },
    draftData : function() {
      var document = this.field().signatory().document();
      var page = document.mainfile() != undefined ? document.mainfile().page(this.get("page")) : undefined;
      var draft = {
        xrel : this.xrel(),
        yrel : this.yrel(),
        wrel : this.wrel(),
        hrel : this.hrel(),
        fsrel : this.fsrel(),
        page : page != undefined ? page.number() : this.get("page"),
        tip : this.get("tip")
      };
      return draft;
    },
    setFSRel : function(fsrel) {
      this.set({"fsrel" : fsrel });
    },
    signatory: function() {
        return this.get('signatory');
    },
    setSignatory: function(s) {
        return this.set({signatory:s});
    },
    die: function() {
        this.set({alive:false});
    },
    alive: function() {
        return this.get('alive');
    },
    bindBubble: function() {
        var placement = this;
        placement.bind('change', placement.bubbleSelf);
        placement.bind('bubble', placement.triggerBubble);
    },
    bubbleSelf: function() {
        var placement = this;
        placement.trigger('bubble');
    },
    triggerBubble: function() {
        var placement = this;
        var field = placement.field();
        if(field)
            field.trigger('bubble');
    },
    step: function() {
        return this.get('step');
    },
    goToStepField : function() {
        this.set({step:'field'});
    },
    goToStepEdit : function() {
        this.set({step:'edit'});
    }
});


})(window);
