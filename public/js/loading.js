/* Loading dialog
 *
 * Open with LoadingDialog.open(some html);
 * Close with LoadingDialog.close();
*/

(function(){

window.LoadingDialog = {
    dialog : function() {
      return ScreenBlockingDialog({'message': message, withSpinner: true});
    },
    changeMessage : function(message) {
      return ScreenBlockingDialog.changeMessage(message);
    },
    changeSubMessage : function(message) {
      return ScreenBlockingDialog.changeSubMessage(message);
    },
    open: function (message, submessage) {
      submessage = submessage || '';
      return ScreenBlockingDialog.open({message: message, submessage: submessage, withSpinner: true});
    },
    close : function() {
      return ScreenBlockingDialog.close();
    }
};
})();

