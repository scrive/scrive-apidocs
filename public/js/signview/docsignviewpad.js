/* Give to next signatory option in sign view
 */


(function(window){

window.PadGiveToNextSignatoryModel = Backbone.Model.extend({
    initialize: function (args) {
       this.setSignatory(args.document.signatoriesThatCanSignNow()[0]);      
    },
    document: function() {
       return this.get("document");
    },
    selectedSignatory: function() {
       return this.get("selectedSignatory");
    },
    setSignatory : function(sig){
        this.set({"selectedSignatory": sig});
    },
    giveForSigning: function() {
        var sig = this.selectedSignatory();
        sig.addtoPadQueue().sendAjax(function() {
                      window.location =  '/padqueue';
                });
    }
});

window.PadGiveToNextSignatoryView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.render();
    },
    sigSelector : function(source,callback) {
        var model = this.model;
        var document = model.document();
        var sigs = document.signatoriesThatCanSignNow();
        var span = $("<span/>").text(localization.pad.changePartyTo);
        if (sigs.length <= 1)
         return span.add($("<strong/>").text(source().smartname() != "" ? source().smartname() : localization.pad.notNamedParty ));
        var select = $("<select/>");
        _.each(sigs,function(sig) {
            var option = $("<option>").text(sig.smartname() != "" ? sig.smartname() :  localization.pad.notNamedParty ).val(sig.email());
            if (sig == source())
                option.attr("selected","YES");
            select.append(option);
        });
        select.change(function() {
            var email = $(this).val();
             _.each(model.document().signatories(),function(sig) {
                if (sig.email() == email)
                    callback(sig);
            });
        });
        return span.add(select);    
        
    },
    render : function() {
           var box = $(this.el);
           var model = this.model;
           box.empty();
           box.addClass('giveForSigningBox');
           var giveForSigningSelector = this.sigSelector(function() {return model.selectedSignatory()}, function(a) {model.setSignatory(a)});
           var giveForSigningButton = Button.init({
                                size: "tiny",
                                color : "green",
                                text : model.document().process().localization().sendbuttontext,
                                //cssClass: "float-right" ,                 
                                onClick : function() {
                                    model.giveForSigning();
                                    return false;
                                }
                              }).input().css("display", "inline-block");
           var giveForSigning =  $("<span class='giveForSigning'/>").append($("<span/>").text(localization.pad.signingOnSameDeviceFor1))
                                                                .append(giveForSigningSelector)
                                                                .append($("<span/>").text(localization.pad.signingOnSameDeviceFor2))
                                                                .append(giveForSigningButton);
      
           box.append(giveForSigning);
           return box;
    }
});

})(window);
