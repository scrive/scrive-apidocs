var Backbone = require("backbone");
var _ = require("underscore");
var PlacementAnchor = require("./anchors.js").PlacementAnchor;

/* Placements of fields for signatories */


var FieldPlacement = exports.FieldPlacement = Backbone.Model.extend({
    defaults: {
      placed : false,
      tip: undefined,
      wrel: 0,
      hrel: 0,
      fsrel: 0.0168,
      alive: true,
      anchors: [],
      highlighted: false
    },
    initialize : function(args){
        var placement = this;
        _.bindAll(this,'remove');
        placement.addToPage();
        if (this.tip() === undefined)
          this.set({"tip" : args.field.defaultTip()});
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
                document.trigger('placements:change');
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
      if (this.get("fsrel") != 0 || this.field().isCheckbox() || this.field().isRadioGroup())
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
      if (this.field() != newField) {
        if (this.field()) {
          this.field().removePlacement(this);
        }
        newField.addPlacement(this);
        this.setField(newField);
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
    remove : function() {
       this.trigger("removed");
       var document = this.field().signatory().document();
       var page = document.file() && document.file().page(this.get("page"));
       if (page != undefined) {
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
    setCheckboxRel : function(wrel) {
      this.set({"wrel" : wrel , "hrel" : 0, "fsrel" : 0}); // hrel and fsres should be set to 0 already
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
    highlighted: function () {
      return this.get("highlighted");
    },
    setHighlighted: function (newHighlighted) {
      this.set("highlighted", newHighlighted);
    }
});

