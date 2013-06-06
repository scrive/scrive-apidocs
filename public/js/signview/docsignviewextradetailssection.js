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
     return field != undefined && !new EmailValidation().validateData(field.value()) && (!field.hasPlacements() || !field.obligatory());
   },
   emailInput : function() {
     var signatory = this.model;
     var field = signatory.emailField();
     var iti = new InfoTextInput({
                                 infotext: localization.email,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");

                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.el();
   },
   nameInputAvaible : function() {
     var signatory = this.model;
     var field1 = signatory.fstnameField();
     var field2 = signatory.sndnameField();
     return field1 != undefined && (field1.value() == "" || field1.value() == undefined) && (!field1.hasPlacements() || !field1.obligatory()) &&
            field2 != undefined && (field2.value() == "" || field2.value() == undefined) && (!field2.hasPlacements() || !field2.obligatory());
   },
   nameInput : function() {
     var signatory = this.model;
     var field = signatory.fstnameField();
     var iti =  new InfoTextInput({
                                 infotext: localization.name,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.el();
   },
   ssnInputAvaible : function() {
     var signatory = this.model;
     var field = signatory.personalnumberField();
     return field != undefined && (field.value() == "" || field.value() == undefined) && (!field.hasPlacements() || !field.obligatory()) && signatory.elegAuthentication();
   },
   ssnInput : function() {
     var signatory = this.model;
     var field = this.model.personalnumberField();
     var iti = new InfoTextInput({
                                 infotext: localization.personamNumber,
                                 value: field.value(),
                                 onChange : function(value) {
                                     field.setValue(value);
                                     signatory.trigger("change");
                                 }
                            });
     field.bind("change", function() {iti.setValue(field.value())});
     return iti.el();
   },
   signatureInputAvaible : function() {
       var signatory = this.model;
       if( signatory.document().currentSignatory().padDelivery() && signatory.hasSignatureField()) {
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
       if (this.nameInputAvaible())
        this.fillBox.append(this.nameInput());
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
