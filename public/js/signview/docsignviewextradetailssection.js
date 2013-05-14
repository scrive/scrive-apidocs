/* Signatory view of document
 * Usage:
 *
 *   $('body').append(new DocumentSignSignSection(model : document).el);
 */


(function(window) {

window.DocumentSignExtraDetailsSection = Backbone.View.extend({
   initialize : function(args){
      this.textstyle = args.textstyle;
      this.render();
   },
   emailInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.emailField();
       return (!signatory.document().emailDelivery() ||
               !new EmailValidation().validateData(field.value())) && (!field.hasPlacements() || !field.obligatory());
   },
   emailInput : function() {
     var signatory = this.model;
     var field = signatory.emailField();
     var iti = InfoTextInput.init({
                                 infotext: localization.email,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");

                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.input();
   },
   fstNameInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.fstnameField();
     return (field.value() == "" || field.value() == undefined) && (!field.hasPlacements() || !field.obligatory());
   },
   fstNameInput : function() {
     var signatory = this.model;
     var field = signatory.fstnameField();
     var iti =  InfoTextInput.init({
                                 infotext: localization.fstname,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.input();
   },
   sndNameInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.sndnameField();
     return (field.value() == "" || field.value() == undefined) && (!field.hasPlacements() || !field.obligatory());
   },
   sndNameInput : function() {
     var signatory = this.model;
     var field = this.model.sndnameField();
     var iti = InfoTextInput.init({
                                 infotext: localization.sndname,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.input();;
   },
   ssnInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.personalnumberField();
     return (field.value() == "" || field.value() == undefined) && (!field.hasPlacements() || !field.obligatory()) && this.model.document().elegAuthentication()
   },
   ssnInput : function() {
     var signatory = this.model;
     var field = this.model.personalnumberField();
     var iti = InfoTextInput.init({
                                 infotext: localization.personamNumber,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.input();
   },
   signatureInputAvaible : function() {
       var signatory = this.model;
       if( signatory.document().padDelivery() ) {
           return ! signatory.anySignatureHasImageOrPlacement();
       }
       else {
           return false;
       }
   },
   signatureInput : function() {
     var signatory = this.model;
     var field = this.model.signatures()[0];
     field.bind("change", function() {signatory.trigger("change");});
     return new SignaturePlacementViewForDrawing({model: field, height : 102, width: 260}).el;
   },
   render: function() {
       var document = this.model;
       var box = $(this.el).addClass('section').addClass('spacing').addClass('extradetails');
       var header = $("<h2 class='title'/>").text(localization.docsignview.filladitionfields);
       header.css(this.textstyle);
       var description = $("<div class='column spacing descriptionbox'/>").text(localization.docsignview.filladitionfieldsdescription);
       description.css(this.textstyle);
       this.fillBox = $("<div class='column spacing fillbox'/>");
       if (this.fstNameInputAvaible())
        this.fillBox.append(this.fstNameInput());
       if (this.sndNameInputAvaible())
        this.fillBox.append(this.sndNameInput());
       if (this.emailInputAvaible())
        this.fillBox.append(this.emailInput());
       if (this.ssnInputAvaible())
        this.fillBox.append(this.ssnInput());
       if (this.signatureInputAvaible())
        this.fillBox.append(this.signatureInput());
       box.append(header).append(description).append(this.fillBox).append("<div class='clearfix' />");
   }
});

})(window);
