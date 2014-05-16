/* Give to next signatory option in sign view
 */

define(['Backbone', 'legacy_code'], function() {

window.PadGiveToNextSignatoryModel = Backbone.Model.extend({
    initialize: function (args) {
       this.setSignatory(args.document.signatoriesThatCanSignNowOnPad()[0]);
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
        LocalStorage.set("pad","from-list","false");
        sig.giveForPadSigning().send();
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
        var sigs = document.signatoriesThatCanSignNowOnPad();
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
           var giveForSigningButton = new Button({
                                size: "tiny",
                                color : "green",
                                style: "display:inline-block;margin-left: 10px" ,
                                text : localization.process.changesignatorybuttontext,
                                onClick : function() {
                                    model.giveForSigning();
                                    return false;
                                }
                              }).el();
           var giveForSigning =  $("<span class='giveForSigning'/>").append(giveForSigningSelector)
                                                                    .append(giveForSigningButton);

           box.append(giveForSigning);
           return box;
    }
});

});
