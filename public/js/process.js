/* Whole document schema
 */


(function(window){
/*
 * Document has id and knows if its signed
 */


window.Process = Backbone.Model.extend({
    defaults : {
      process : "Contract"
    },
    name : function() {
        return this.get("process");
    },
    process : function() {
        return this.get("process");
    },
    isContract : function() {
      return this.process() == "Contract";
    },
    isOffer : function() {
      return this.process() == "Offer";
    },
    isOrder : function() {
      return this.process() == "Order";
    },
    changeToContract : function() {
      this.set({"process": "Contract"});
    },
    changeToOffer : function() {
       this.set({"process": "Offer"});
    },
    changeToOrder : function() {
       this.set({"process": "Order"});
    },
    processLocalization: function() {
        if (this.isOffer())
            return {signatorysignmodalcontentdesignvieweleg: localization.process.offer.signatorysignmodalcontentdesignvieweleg,
                    signatorysignmodalcontentsignvieweleg: localization.process.offer.signatorysignmodalcontentsignvieweleg,
                    signatorysignmodalcontentauthoronly: localization.process.offer.signatorysignmodalcontentauthoronly,
                    signatorycancelmodaltitle: localization.process.offer.signatorycancelmodaltitle,
                    signatorysignmodalcontent: localization.process.offer.signatorysignmodalcontent,
                    signatorysignmodaltitle: localization.process.offer.signatorysignmodaltitle,
                    remindagainbuttontext: localization.process.offer.remindagainbuttontext,
                    authorsignatoryname: localization.process.offer.authorsignatoryname,
                    restartbuttontext: localization.process.offer.restartbuttontext,
                    prolongbuttontext: localization.process.offer.prolongbuttontext,
                    confirmsendtitle: localization.process.offer.confirmsendtitle,
                    rejectbuttontext: localization.process.offer.rejectbuttontext,
                    nonsignatoryname: localization.process.offer.nonsignatoryname,
                    cancelbuttontext: localization.process.offer.cancelbuttontext,
                    cancelmodaltitle: localization.process.offer.cancelmodaltitle,
                    confirmsendtext: localization.process.offer.confirmsendtext,
                    cancelmodaltext: localization.process.offer.cancelmodaltext,
                    signbuttontext: localization.process.offer.signbuttontext,
                    sendbuttontext: localization.process.offer.sendbuttontext,
                    signatoryname: localization.process.offer.signatoryname,
                    expirytext: localization.process.offer.expirytext,
                    authorname: localization.process.offer.authorname};
        else if (this.isOrder())
            return {signatorysignmodalcontentdesignvieweleg: localization.process.order.signatorysignmodalcontentdesignvieweleg,
                    signatorysignmodalcontentsignvieweleg: localization.process.order.signatorysignmodalcontentsignvieweleg,
                    signatorysignmodalcontentauthoronly: localization.process.order.signatorysignmodalcontentauthoronly,
                    signatorycancelmodaltitle: localization.process.order.signatorycancelmodaltitle,
                    signatorysignmodalcontent: localization.process.order.signatorysignmodalcontent,
                    signatorysignmodaltitle: localization.process.order.signatorysignmodaltitle,
                    remindagainbuttontext: localization.process.order.remindagainbuttontext,
                    authorsignatoryname: localization.process.order.authorsignatoryname,
                    restartbuttontext: localization.process.order.restartbuttontext,
                    prolongbuttontext: localization.process.order.prolongbuttontext,
                    confirmsendtitle: localization.process.order.confirmsendtitle,
                    rejectbuttontext: localization.process.order.rejectbuttontext,
                    nonsignatoryname: localization.process.order.nonsignatoryname,
                    cancelbuttontext: localization.process.order.cancelbuttontext,
                    cancelmodaltitle: localization.process.order.cancelmodaltitle,
                    confirmsendtext: localization.process.order.confirmsendtext,
                    cancelmodaltext: localization.process.order.cancelmodaltext,
                    signbuttontext: localization.process.order.signbuttontext,
                    sendbuttontext: localization.process.order.sendbuttontext,
                    signatoryname: localization.process.order.signatoryname,
                    expirytext: localization.process.order.expirytext,
                    authorname: localization.process.order.authorname};
        else
            return {signatorysignmodalcontentdesignvieweleg: localization.process.contract.signatorysignmodalcontentdesignvieweleg,
                    signatorysignmodalcontentsignvieweleg: localization.process.contract.signatorysignmodalcontentsignvieweleg,
                    signatorysignmodalcontentauthoronly: localization.process.contract.signatorysignmodalcontentauthoronly,
                    signatorycancelmodaltitle: localization.process.contract.signatorycancelmodaltitle,
                    signatorysignmodalcontent: localization.process.contract.signatorysignmodalcontent,
                    signatorysignmodaltitle: localization.process.contract.signatorysignmodaltitle,
                    remindagainbuttontext: localization.process.contract.remindagainbuttontext,
                    authorsignatoryname: localization.process.contract.authorsignatoryname,
                    restartbuttontext: localization.process.contract.restartbuttontext,
                    prolongbuttontext: localization.process.contract.prolongbuttontext,
                    confirmsendtitle: localization.process.contract.confirmsendtitle,
                    rejectbuttontext: localization.process.contract.rejectbuttontext,
                    nonsignatoryname: localization.process.contract.nonsignatoryname,
                    cancelbuttontext: localization.process.contract.cancelbuttontext,
                    cancelmodaltitle: localization.process.contract.cancelmodaltitle,
                    confirmsendtext: localization.process.contract.confirmsendtext,
                    cancelmodaltext: localization.process.contract.cancelmodaltext,
                    signbuttontext: localization.process.contract.signbuttontext,
                    sendbuttontext: localization.process.contract.sendbuttontext,
                    signatoryname: localization.process.contract.signatoryname,
                    expirytext: localization.process.contract.expirytext,
                    authorname: localization.process.contract.authorname};
    },
    numberedsignatories : function() {
       return true;
    }
});

window.Lang = Backbone.Model.extend({
    lang : function() {
        return this.get("lang");
    },
    simpleCode : function() {
      if (this.en())
        return "en";
      else if (this.sv())
        return "sv";
    },
    en : function(){
        return this.lang() == "en" || this.lang() == "gb";
    },
    sv : function(){
        return this.lang() == "sv" || this.lang() == "se";
    },
    setEN : function() {
        this.set({lang : "en"});
    },
    setSV : function() {
        this.set({lang : "sv"});
    },
    draftData : function() {
        return this.lang();
    }
});

})(window);
