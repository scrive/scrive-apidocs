/* Fields of signatories
 * Placement of fields + support for moving them to files
 * View of field in sign view 
 */


(function(window){

window.FieldPlacement = Backbone.Model.extend({
    defaults: {
        x : 0,
        y : 0,
        placed : false
        
    },
    initialize : function(args){
        var placement = this;
        setTimeout(function() {placement.addToPage();},100);
    },
    placed : function() {
          return this.get("placed");
    },
    addToPage : function() {
         if (this.placed()) return;
         var placement = this;
         var document = placement.field().signatory().document();
         var fileid = this.get("fileid");
         if ( document.getFile(fileid) != undefined)
         {
             var page = document.getFile(fileid).page(this.get("page"));
             if (page != undefined)
             {
                this.set({placed : true});
                page.addPlacement(placement);
             }
                      
          }
         return setTimeout(function() {placement.addToPage();},100);
         
    },
    x : function() {
        return this.get("x");
    },
    y : function() {
        return this.get("y");
    },
    page : function() {
        return this.get("page");
    },
    field : function(){
        return this.get("field");
    },
    file : function(){
        return this.get("file");
    }
});

window.Field = Backbone.Model.extend({
    defaults: {
        name : "",
        value : "",
        closed : false,
        placements : []
    },
    initialize : function(args){
        var field = this;
        var extendedWithField =   function(hash){
                    hash.field = field;
                    return hash;
        };
        var placements =  _.map(args.placements, function(placement){
                return new FieldPlacement(extendedWithField(placement));
        });
        this.set({"placements": placements});
    },
    name : function() {
        return this.get("name");
    },
    value : function() {
        return this.get("value");
    },
    setValue : function(value) {
        return this.set({value : value});
    },
    isClosed : function() {
        return this.get("closed");
    },
    placements : function() {
        return this.get("placements");
    },
    signatory : function(){
        return this.get("signatory");
    },
    canBeIgnored: function(){
        var name = this.name();
        return this.value() == "" && this.placements().length == 0 && (name == "sigco" || name == "sigpersnr" || name == "sigcompnr")
    },
    readyForSign : function(){
        return this.value() != "" || this.canBeIgnored();
    },
    nicename : function() {
        var name = this.name();
        if (name == "sigco")
            return localization.company;
        if (name == "sigpersnr" )     
            return localization.personamNumber;
        if (name == "sigcompnr") 
            return localization.companyNumber;
        return name;
    }
   
});


window.FieldStandardView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'cleanredborder');
        this.model.view = this;
        this.model.bind('change', this.cleanredborder);
        this.render();
    },
    render: function(){
        var field = this.model;
        this.el.empty();
        var signatory = field.signatory();
        if (!signatory.canSign() || field.isClosed() || !signatory.current()) {
            this.el.addClass("field");
            input = $("<input class='fieldvalue' autocomplete='off' readonly=''/>");
            if (field.value() != "")
                input.val(field.value());
            else
                {
                input.val(field.nicename());
                input.addClass("grayed");
                }
        this.el.append(input);
        }
        else
        {
            this.el.addClass("dragfield");
            var wrapper = $("<div style='border: medium none ! important;' class='dragfield'/>");
            var input = InfoTextInput.init({
                                 infotext: field.nicename(),
                                 value: field.value(),
                                 cssClass :'fieldvalue',       
                                 onChange : function(value) {
                                    field.setValue(value);    
                                  }
                            }).input();
            this.redborderhere = input;                            
            input.attr("autocomplete","off");                
            wrapper.append(input);
            this.el.append(wrapper);    
        }
        
        return this;
    },
    redborder : function() {
        this.redborderhere.css("border","1px solid red");
    },
    cleanredborder : function() {
        this.redborderhere.css("border","");
    }
});

    
})(window); 
