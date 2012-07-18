/* Signatory view of document
 */


(function(window) {

window.DocumentSignSignSection = Backbone.View.extend({
   initialize : function(args){
      this.render();
   },
   tagname : 'div',
   render: function() {
       var model = this.model;
       var document = this.model.document();
       var box = $(this.el).addClass('section').addClass('spacing').addClass('signbuttons');
       this.rejectButton = Button.init({
                                        size: "big",
                                        color: "red",
                                        text: document.process().rejectbuttontext(),
                                        onClick: function() {
                                            ConfirmationWithEmail.popup({
                                            title: document.process().signatorycancelmodaltitle(),
                                            mail: document.currentSignatory().rejectMail(),
                                            acceptText: localization.reject.send,
                                            editText: localization.reject.editMessage,
                                            rejectText: localization.cancel,
                                            acceptColor: "red",
                                            onAccept: function(customtext) {
                                                document.currentSignatory().reject(customtext).send();
                                                }
                                            });
                                        }
                                });
       this.signButton = Button.init({
                            size: "big",
                            color: "blue",
                            text: document.process().signbuttontext(),
                            icon: $("<span class='icon cross'></span>"),
                            onClick: function() {
                                var valid =  model.tasks().notCompleatedTasks().length == 1 && model.tasks().notCompleatedTasks()[0] == model.signtask();
                                if (!valid) {
                                        model.arrowview().blink();
                                        return false;
                                    }
                                new DocumentSignConfirmation({
                                    model: document
                                    }).popup();
                                }
                            });
      box.append($("<div class='rejectwrapper'>").append(this.rejectButton.input()));
      box.append($("<div class='signwrapper'>").append(this.signButton.input()));
      box.append($("<div class='clearfix' />"));
   } 
});

})(window);
