/* This allows to check some padoptions
 */


(function(window){

var PadDesignViewUtilsModel = Backbone.Model.extend({
 defaults: {
        giveForSigning : false,
        sendForSigning : false
    },
    initialize: function (args) {
        var sig = args.document.signatoriesThatCanSignNow()[0];
        this.setGiveForSigningSignatory(sig);
        this.setSendToPadSignatory(sig);      
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
    setGiveForSigning : function(){
       this.set({giveForSigning: true, sendToPad: false });
    },
    sendToPad: function(){
        return this.get("sendToPad");
    },
    setSendToPad : function(){
       this.set({sendToPad : true, giveForSigning : false});
    },
    justSend : function() {
        return ! this.giveForSigning() && ! this.sendToPad(); 
    },
    setJustSend : function() {
       this.set({sendToPad : false, giveForSigning : false});
    },
    postSendAction : function(link) {
       var padDesignViewUtilsModel = this;
       var spsignatory = this.sendToPadSignatory();
       var gssignatory = this.giveForSigningSignatory();
       LoadingDialog.changeMessage(localization.designview.messages.reloadingDocument);
       var ndocument = new Document({id : this.document().documentid()});
       ndocument.bind('change', function() {
              if (!ndocument.ready()) return;
              LoadingDialog.changeMessage(localization.designview.messages.extraActions);
              _.each(ndocument.signatories(), function(sig) {
                if (sig.email() == spsignatory.email())
                         spsignatory = sig;
                if (sig.email() == gssignatory.email())
                         gssignatory = sig;
              });
              if (spsignatory.signatoryid() != 0 && padDesignViewUtilsModel.sendToPad())
                  spsignatory.addtoPadQueue().sendAjax(function() {
                      window.location = link;                      
                });
              else if (gssignatory.signatoryid() != 0 &&  padDesignViewUtilsModel.giveForSigning())
                  gssignatory.addtoPadQueue().sendAjax(function() {
                      window.location = gssignatory.padSigningURL();               
                });
              else
                 window.location = link;
       });
       ndocument.recall();
   }           
});

var PadDesignViewUtilsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.render();
    },
    sigSelector : function(source,callback) {
        var model = this.model;
        var sigs = model.document().signatoriesThatCanSignNow();
        var options = [];
        _.each(sigs,function(sig) {
          if (sig != source()) {
            var name = sig.smartname();
            if (name == undefined || name == "") name = "(" + localization.pad.notNamedParty + ")";
            options.push({name : name, onSelect : function() { callback(sig); } });
          }
        });
        var name = source().smartname();
        if (name == undefined || name == "") name = "(" + localization.pad.notNamedParty + ")";
        var select = new Select({options : options, name : name, cssClass : "float-left" });
        return select.view().el;    
        
    },
    render : function() {
           var box = $(this.el);
           var model = this.model;
           box.empty();
           box.addClass('padoptions');
           var justSendRadio = $("<input type='radio' name='padsend'/>");
           var justSendRadioLabel =  $("<span class='label'/>").text(localization.pad.justSend);

           var justSend = $("<div class='padoption'/>").append(justSendRadio).append(justSendRadioLabel);

           if (model.justSend())
               justSendRadio.attr("checked","checked");
           justSendRadio.change(function() {
               model.setJustSend();
            });
           
           var giveForSigningRadio = $("<input type='radio' name='padsend'/>");
           var giveForSigningSelector = this.sigSelector(function() {return model.giveForSigningSignatory()}, function(a) {model.setGiveForSigningSignatory(a)});
           var giveForSigningLabel =  $("<span class='label'/>").append($("<span class='float-left'/>").text(localization.pad.signingOnSameDeviceFor));
           var giveForSigning = $("<div class='padoption'/>").append(giveForSigningRadio)
                                                             .append(giveForSigningLabel)
                                                             .append(giveForSigningSelector);
                                                            
           if (model.giveForSigning())
               giveForSigningRadio.attr("checked","checked");
           giveForSigningRadio.change(function() {
               model.setGiveForSigning();
            });

           
           var sendToPadRadio = $("<input type='radio' name='padsend'/>");
           var sendToPadSelector = this.sigSelector(function() {return model.sendToPadSignatory()}, function(a) {model.setSendToPadSignatory(a)});
           var sendToPadLabel =  $("<span class='label'/>").append($("<span class='float-left'/>").text(localization.pad.sendToDifferentDeviceFor));
           var sendToPad = $("<div class='padoption'/>").append(sendToPadRadio)
                                                        .append(sendToPadLabel)
                                                        .append(sendToPadSelector);

           if (model.sendToPad())
               sendToPadRadio.attr("checked","checked");
           sendToPadRadio.change(function() {
               model.setSendToPad();
            });

            box.append(justSend);
            box.append(giveForSigning);
            box.append(sendToPad);
           return this; 
    }
});

window.PadDesignViewUtils = function(args) {
          var model = new PadDesignViewUtilsModel({document : args.document});
          var view = new PadDesignViewUtilsView({model : model});
          return new Object({
              el : function()  {return $(view.el);},
              postSendAction : function(args) {model.postSendAction(args);}
            });
};

})(window);
