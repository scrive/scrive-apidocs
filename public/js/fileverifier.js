/* File input that after upload will show information if file is signed by GuardTime
 *    
 */
  

(function( window){
var FileVerifierModel = Backbone.Model.extend({});

var FileVerifierView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    showResultDialog : function(res) {
          var dialog = $("<div class='overlay verificationModal'></div>"); 
          var container = $("<div class='modal-container'></div>");
          var close = $("<div class='modal-close close'></div>");
          var body = $("<div class='modal-body' style='padding:10px;text-align: center'>");
          var title = "";
          var bleft = $("<div class='float-left' style='width:60px;text-align: center'/>");
          var bright = $("<div class='float-right'  style='width:180px;text-align: left'/>");
          if (res.success) {
              bleft.append("<div class='verificationSuccessIcon'>");
              title = localization.verification.success;
              bright.append($("<div/>").text(localization.verification.time + ": " + res.time));
              bright.append("<BR/>");
              bright.append($("<div/>").text(localization.verification.gateway + ": " + res.gateway));
          }    
          else if (res.error)  {
              bleft.append("<div class='verificationErrorIcon' style='margin-top: 25px;'>");
              title = localization.verification.error;
              bright.append("<BR/>");
              bright.append($("<div/>").text(localization.verification.errorMessage));
              dialog.addClass("failed");
          }
          else  {              
              bleft.append("<div class='verificationFailedIcon' style='margin-top: 25px;'>");
              title = localization.verification.failed;
              bright.append("<BR/>");
              bright.append($("<div/>").text(localization.verification.failedMessage));
              dialog.addClass("failed");
          }                                  
          Confirmation.popup({title: title, content : $("<div style='height:100px;width: 300px;margin: auto;'/>").append(bleft).append(bright)});
        
    },
    uploadButton : function() {
        var view = this;
        return  UploadButton.init({
            name: "file",
            width: 380,
            size: "big",
            text: localization.uploadButton,
            submitOnUpload: true,
            onClick : function () {
              LoadingDialog.open();
            },
            onError: function() {
              LoadingDialog.close();
            },
            submit: new Submit({
              method : "POST",
              ajax: true,
              url : "/verify",
              onSend: function() {
                LoadingDialog.open();
              },
              ajaxerror: function(d,a){
                LoadingDialog.close();
                view.render();
              },
              ajaxsuccess: function(res) {
                LoadingDialog.close();
                view.showResultDialog(JSON.parse(res));
                view.render();
              }
            })
          });
    },
    render: function () {
        var box = $(this.el);
        box.html(this.uploadButton().input());
        return box;
    }
});

window.FileVerifier = {
    init: function (args) {
          var model = new FileVerifierModel();
          var view = new FileVerifierView({model : model, el: $("<div/>")});
          return new Object({
              model : model,
              view : view
            });
        }
};

})(window); 
