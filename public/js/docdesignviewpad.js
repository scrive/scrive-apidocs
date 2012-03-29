/* This allows to check some padoptions
 */


(function(window){

window.PadDesignViewUtilsModel = Backbone.Model.extend({
 defaults: {
        //giveForSigningSignatory: undefined, Set
        //sendForPadSignatory
        giveForSigning : false,
        sendForSigning : false
    },
    initialize: function (args) {
        var sigs = args.document.signatories();
        for(var i=sigs.length-1;i>0;i--){
            if (sigs[i].signs())
            {
                this.setGiveForSigningSignatory(sigs[i]);
                this.setSendToPadSignatory(sigs[i]);
                break;
            }
        }
            
        
    },
    document: function() {
       return this.get("document");
    },
    setGiveForSigningSignatory : function(sig){
        this.set({"giveForSigningSignatory": sig});
    },
    giveForSigningSignatory: function() {
       return this.get("giveForSigningSignatory");
    },
    setSendToPadSignatory : function(sig){
        this.set({"sendToPadSignatory": sig});
    },
    sendToPadSignatory: function() {
       return this.get("sendToPadSignatory");
    },
    giveForSigning : function(){
       return this.get("giveForSigning");
    },
    toogleGiveForSigning : function(){
       this.set({giveForSigning: !this.giveForSigning(), sendToPad: false });
    },
    sendToPad: function(){
        return this.get("sendToPad");
    },
    toogleSendToPad : function(){
       this.set({sendToPad : !this.sendToPad(), giveForSigning : false});
    },
    postSendAction : function(link) {
       var padDesignViewUtilsModel = this;
       var spsignatory = this.sendToPadSignatory();
       var gssignatory = this.giveForSigningSignatory();
       LoadingDialog.changeMessage(localization.designview.messages.reloadingDocument)
       var ndocument = new Document({id : this.document().documentid()});
       ndocument.bind('change', function() {
              if (!ndocument.ready()) return;
              LoadingDialog.changeMessage(localization.designview.messages.aditionalActions);
              _.each(ndocument.signatories(), function(sig) {
                if (sig.email() == spsignatory.email())
                         spsignatory = sig;
                if (sig.email() == gssignatory.email())
                         gssignatory = sig;
              });
              if (spsignatory.signatoryid() != 0 && padDesignViewUtilsModel.sendToPad())
                  spsignatory.addtoPadQueue().sendAjax(function() {
                      window.location = link                      
                });
              else if (gssignatory.signatoryid() != 0 &&  padDesignViewUtilsModel.giveForSigning())
                  gssignatory.addtoPadQueue().sendAjax(function() {
                      window.open(gssignatory.padSigningURL(),'Pad signing');
                      window.location = window.location;                     
                });
       });
       ndocument.recall();
   }           
});

window.PadDesignViewUtilsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.render();
    },
    tagname : "div",
    sigSelector : function(source,callback) {
        var model = this.model;
        var sigs = model.document().signatories();
        if (model.document().signatories().length == 2)
         return $("<strong/>").text(source().smartname());
        var select = $("<select/>");
        _.each(model.document().signatories(),function(sig) {
            var option = $("<option>").text(sig.smartname()).val(sig.email())
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
        })
        return select;    
        
    },
    render : function() {
           var box = $(this.el);
           var model = this.model;
           box.empty();
           box.addClass('padoptions');
           var giveForSigningRadio = $("<input type='radio' name='padsend'/>")
           var giveForSigningSelector = this.sigSelector(function() {return model.giveForSigningSignatory()}, function(a) {model.setGiveForSigningSignatory(a)});
           var giveForSigningLabel =  $("<span class='label'/>").append($("<span/>").text(localization.pad.signingOnSameDeviceFor)).append(giveForSigningSelector);
           var giveForSigning = $("<div class='padoption'/>").append(giveForSigningRadio)
                                                             .append(giveForSigningLabel)
                                                            
           if (model.giveForSigning())
               giveForSigningRadio.attr("checked","checked");
           giveForSigningRadio.change(function() {
               model.toogleGiveForSigning();
            })

           
           var sendToPadRadio = $("<input type='radio' name='padsend'/>")
           var sendToPadSelector = this.sigSelector(function() {return model.sendToPadSignatory()}, function(a) {model.setSendToPadSignatory(a)})
           var sendToPadLabel =  $("<span class='label'/>").append($("<span/>").text(localization.pad.addToPadQueueFor)).append(sendToPadSelector);
           var sendToPad = $("<div class='padoption'/>").append(sendToPadRadio)
                                                        .append(sendToPadLabel)

           if (model.sendToPad())
               sendToPadRadio.attr("checked","checked");
           sendToPadRadio.change(function() {
               model.toogleSendToPad();
            })
           box.append(giveForSigning).append(sendToPad);
           return this; 
    }
});

})(window);
