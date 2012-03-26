/* Whole document schema
 */


(function(window){
/*
 * Document has id and knows if its signed
 */


window.Process = Backbone.Model.extend({
    name : function(){
        return this.get("name");
    },
    title : function(){
        return this.get("title");
    },
    basicavailable : function(){
        return this.get("basicavailable");
    },
    authorsend : function(){
        return this.get("authorsend");
    },
    validationchoiceforbasic : function(){
        return this.get("validationchoiceforbasic");
    },
    expiryforbasic : function(){
        return this.get("expiryforbasic");
    },
    step1text : function(){
        return this.get("step1text");
    },
    expirywarntext : function(){
        return this.get("expirywarntext");
    },
    sendbuttontext : function(){
        return this.get("sendbuttontext");
    },
    confirmsendtitle : function(){
        return this.get("confirmsendtitle");
    },
    confirmsendtext : function(){
        return this.get("confirmsendtext");
    },
    expirytext : function(){
        return this.get("expirytext");
    },
    restartbuttontext : function(){
        return this.get("restartbuttontext");
    },
    cancelbuttontext : function(){
        return this.get("cancelbuttontext");
    },
    rejectbuttontext : function(){
        return this.get("rejectbuttontext");
    },
    cancelmodaltitle : function(){
        return this.get("cancelmodaltitle");
    },
    cancelmodaltext : function(){
        return this.get("cancelmodaltext");
    },
    authorissecretarytext : function(){
        return this.get("authorissecretarytext");
    },
    remindagainbuttontext : function(){
        return this.get("remindagainbuttontext");
    },
    requiressignguard : function(){
        return this.get("requiressignguard");
    },
    signbuttontext :  function(){
        return this.get("signbuttontext");
    },
    signatorycancelmodaltitle : function() {
        return this.get("signatorycancelmodaltitle");
    },
    signguardwarntext :  function() {
        return this.get("signguardwarntext");
    },
    signatorysignmodalcontentnotlast:  function() {
        return this.get("signatorysignmodalcontentnotlast");
    },
    signatorysignmodalcontentauthorlast:  function() {
        return this.get("signatorysignmodalcontentauthorlast");
    },
    signatorysignmodalcontentlast:  function() {
        return this.get("signatorysignmodalcontentlast");
    },
    signatorysignmodaltitle : function() {
        return this.get("signatorysignmodaltitle");
    },
    step1text : function() {
        return this.get("step1text");
    },
    authorsignlastbutton : function() {
        return this.get("authorsignlastbutton");
    },
    authorname : function() {
        return this.get("authorname");
    },
    authorsignatoryname : function() {
        return this.get("authorsignatoryname");
    },
    signatoryname : function() {
        return this.get("signatoryname");
    },
    nonsignatoryname : function() {
        return this.get("nonsignatoryname");
    },
    numberedsignatories : function() {
        return this.get("numberedsignatories");
    }
});

window.Region = Backbone.Model.extend({
    haspeopleids : function(){
        return this.get("haspeopleids");
    },
    iselegavailable : function(){
        return this.get("iselegavailable");
    },
    gb : function(){
        return this.get("gb");
    },
    se : function(){
        return this.get("se");
    },
    setGB : function() {
        this.set({gb: true, se: false});
    },
    setSE : function() {
        this.set({gb: false, se: true});
    },
    draftData : function() {
        if (this.gb())
            return "gb";
        return "se";
    }
});

})(window);
