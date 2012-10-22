/* Signatory view of document
 * Usage:
 * 
 *   $('body').append(new DocumentSignSignSection(model : document).el);
 */


(function(window) {

window.DocumentSignExtraDetailsSection = Backbone.View.extend({
   initialize : function(args){
      this.render();
   },
   emailInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.emailField();
     return (!new EmailValidation().validateData(field.value())) && !field.hasPlacements()
   },
   emailInput : function() {
     var signatory = this.model;
     var field = signatory.emailField();
     return InfoTextInput.init({
                                 infotext: localization.email,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");

                                 }
                            }).input();
   },
   fstNameInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.fstnameField();
     return (field.value() == "" || field.value() == undefined) && !field.hasPlacements()
   },
   fstNameInput : function() {
     var signatory = this.model;
     var field = signatory.fstnameField();
     return InfoTextInput.init({
                                 infotext: localization.fstname,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            }).input();
   },
   sndNameInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.sndnameField();
     return (field.value() == "" || field.value() == undefined) && !field.hasPlacements()
   },
   sndNameInput : function() {
     var signatory = this.model;
     var field = this.model.sndnameField();
     return InfoTextInput.init({
                                 infotext: localization.sndname,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            }).input();
   },
   ssnInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.personalnumberField();
     return (field.value() == "" || field.value() == undefined) && !field.hasPlacements() && this.model.document().elegAuthentication()
   },
   ssnInput : function() {
     var signatory = this.model;
     var field = this.model.personalnumberField();
     return InfoTextInput.init({
                                 infotext: localization.personamNumber,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            }).input();
   },
   fillBoxEl : function() {
      return $(this.fillBox);
   },
   render: function() {
       var document = this.model;
       var box = $(this.el).addClass('section').addClass('spacing').addClass('extradetails');
       var header = $("<h2 class='title'>").text(localization.docsignview.filladitionfields);
       var description = $("<div class='column spacing descriptionbox'>").text(localization.docsignview.filladitionfieldsdescription);
       this.fillBox = $("<div class='column spacing fillbox'>");
       if (this.fstNameInputAvaible())
        this.fillBox.append(this.fstNameInput());
       if (this.sndNameInputAvaible())
        this.fillBox.append(this.sndNameInput());
       if (this.emailInputAvaible())
        this.fillBox.append(this.emailInput());
       if (this.ssnInputAvaible())
        this.fillBox.append(this.ssnInput());
       box.append(header).append(description).append(this.fillBox).append("<div class='clearfix' />");
   } 
});

})(window);
