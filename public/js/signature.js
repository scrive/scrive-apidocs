/* Signature - format, parsing, to-and-from field conversion
 * 
 */


(function(window){

window.Signature = Backbone.Model.extend({
    defaults: {
        width :  260,
        height : 102
        //image : undefined
        //field : undefined

    },
    initialize : function(args){
        var signature = this;
        this.restoreFromField();   
        args.field.bind('change', function() {
               signature.restoreFromField();
            });

    },
    hasImage : function() {
       return this.image() != undefined && this.image() != "";
    },
    image : function() {
        return this.get("image");
    },
    setImage : function(image) {
        this.set({image: image});
        this.saveToField();
    },
    width : function(){
        return this.get("width");
    },
    height : function(){
        return this.get("height");
    },
    swidth : function(){
        return Math.floor(this.width() * this.drawScaling());
    },
    sheight : function(){
        return Math.floor(this.height() * this.drawScaling());
    },
    setSize: function(width, height) {
        this.set({width: Math.floor(width), height : Math.floor(height), image : undefined});
        this.saveToField();
    },
    restoreFromField : function() {
       var values = this.field().value().split("|");
       if (values.length == 3)
          this.set({width: values[0], height : values[1], image : values[2]});
    },
    saveToField: function () {
       var value = this.width() + "|" + this.height() + "|" + (this.hasImage() ? this.image() : "")
       this.field().setValue(value);
    },
    field : function(){
        return this.get("field");
    },
    document : function() {
        return this.field().signatory().document();  
    },
    drawScaling: function() { /*
       if (this.width() <= 190 && this.height() <= 100)
            return 5;
       if (this.width() <= 240 && this.height() <= 100)
            return 4;
       if (this.width() <= 300&& this.height() <= 200)
            return 3;
       if (this.width() <= 450&& this.height() <= 100)
            return 2;
       return 1; */
       return 820/this.width()
       
    }
});


})(window);
