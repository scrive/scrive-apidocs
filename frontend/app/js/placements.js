/* Placements of fields for signatories */

define(['Backbone', 'legacy_code'], function() {

window.FieldPlacement = Backbone.Model.extend({
    defaults: {
      placed : false,
      tip: undefined,
      wrel: 0,
      hrel: 0,
      fsrel: 0.0168,
      withTypeSetter : false,
      alive: true,
      anchors: [],
      step: 'edit'
    },
    initialize : function(args){
        var placement = this;
        _.bindAll(this,'remove');
        placement.addToPage();
        if (this.tip() === undefined)
          this.set({"tip" : args.field.defaultTip()});
        this.setSignatory(args.field.signatory());
        this.setField(args.field);
        var anchors =  _.map(args.anchors, function(anchorargs){
          return new PlacementAnchor(anchorargs);
        });
        this.setAnchors(anchors);
        this.bindBubble();
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
      if (this.get("fsrel") != 0)
        return this.get("fsrel");
      else
        return 0.0168;
    },
    page : function() {
        return this.get("page");
    },
    field : function(){
        return this.get("field");
    },
    anchors: function () {
      return this.get("anchors");
    },
    setAnchors: function (anchors) {
      this.set({ anchors: anchors });
    },
    changeField: function (newField) {
      var field = this.field();
      var sig = newField.signatory();
      var name = newField.name();

      field.removePlacement(this);
      this.setSignatory(sig);
      newField.addPlacement(this);
      this.setField(newField);

      if (!sig.hasTextFieldWithName(name)) {
        sig.addField(newField);
      }
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
       if (page !== undefined) {
         // page can be undefined if the field was placed in template
         // on a second page, but after starting from template, doc
         // was switched to a single page pdf
         page.removePlacement(this);
       }
       this.set({placed:false});
       this.field().removePlacement(this);
       this.off();
       this.die();
    },
    removeField: function () {
      var placement = this;
      var field = placement.field();
      var sig = field.signatory();

      if (field && field.addedByMe && field.value() === "" && field.placements().length <= 1) {
        sig.deleteField(field);
        placement.setField(undefined);
      }
    },
    fixWHRel : function(w,h) {
      var page =  this.field().signatory().document().mainfile().page(this.get("page"));
      if (page != undefined && page.width() != undefined && page.height() != undefined) {
          this.set({ wrel : (w / page.width()) , hrel : (h / page.height()) },    {silent : true});
      }
    },
    draftData : function() {
      var document = this.field().signatory().document();
      var page = document.mainfile() != undefined ? document.mainfile().page(this.get("page")) : undefined;
      var anchors = _.map(this.get("anchors"), function (anchor) { return anchor.draftData(); });
      var draft = {
        xrel : this.xrel(),
        yrel : this.yrel(),
        wrel : this.wrel(),
        hrel : this.hrel(),
        fsrel : this.fsrel(),
        page : page != undefined ? page.number() : this.get("page"),
        tip : this.get("tip"),
        anchors : anchors
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
        placement.bind('change', function() {
          if(placement.field()) {
            placement.field().trigger('change');
          }
        });
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

});
