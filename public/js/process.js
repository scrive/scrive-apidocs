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
    corename : function() { // The name of process used in comunication with server
        return this.get("corename");
    },
    title : function(){
        return this.get("title");
    },
    authorsend : function(){
        return this.get("authorsend");
    },
    step1text : function(){
        return this.get("step1text");
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
    signatorysignmodalcontent:  function() {
        return this.get("signatorysignmodalcontent");
    },
    signatorysignmodalcontentdesignvieweleg:  function() {
        return this.get("signatorysignmodalcontentdesignvieweleg");
    },
    signatorysignmodalcontentsignvieweleg:  function() {
        return this.get("signatorysignmodalcontentsignvieweleg");
    },
    authorIsOnlySignatory:  function() {
        return this.get("signatorysignmodalcontentauthoronly");
    },
    signatorysignmodaltitle : function() {
        return this.get("signatorysignmodaltitle");
    },
    step1text : function() {
        return this.get("step1text");
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
    region : function() {
        return this.get("region");
    },
    gb : function(){
        return this.region() == "gb";
    },
    se : function(){
        return this.region() == "se";
    },
    setGB : function() {
        this.set({region : "gb"});
    },
    setSE : function() {
        this.set({region : "se"});
    },
    draftData : function() {
        return this.region();
    }
});

})(window);
