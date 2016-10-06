var Backbone = require("backbone");
var React = require("react");
var UploadButton = require("../scripts/common/uploadbutton");
var _ = require("underscore");
var $ = require("jquery");
var Confirmation = require("./react_confirmations.js");
var Submit = require("./submits.js").Submit;
var LoadingDialog = require("./loading.js").LoadingDialog;

/* File input that after upload will show information if file is signed by GuardTime
 *
 */


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
          var bright = $("<div class='float-right'  style='width:450px;text-align: left'/>");
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

          var modalContent = $("<div style='height:100px;width: 546px;margin: auto;'/>").append(bleft).append(bright);
          new Confirmation({
            title: title,
            content: modalContent,
            clearOnAccept: true
          });

    },
    uploadButton : function() {
        var view = this;
        var div = $("<div/>");
        React.render(React.createElement(UploadButton,{
          name: "file",
          type: "action",
          width: 380,
          size: "big",
          text: localization.uploadButton,
          submitOnUpload: true,
          onUploadComplete: function(input) {
            var submit = new Submit({
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
                view.showResultDialog(res);
                view.render();
              }
            });
            submit.addInputs(input);
            submit.send();
          }
        }), div[0]);
        return div;
    },
    render: function () {
        var box = $(this.el);
        box.html(this.uploadButton());
        return box;
    }
});

var FileVerifier = exports.FileVerifier = {
    init: function (args) {
          var model = new FileVerifierModel();
          var view = new FileVerifierView({model : model, el: $("<div/>")});
          return new Object({
              model : model,
              view : view
            });
        }
};

