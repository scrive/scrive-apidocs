/* Loading dialog
 *
 * Open with LoadingDialog.open(args).
 * args is an object with the following keys:
 * - header, a HTML string/jQuery object that will be displayed in the loading dialog.
 * - subheader, a HTML string/jQuery object that will be displayed in the loading dialog, slightly smaller then the header.
 *
 * Close with LoadingDialog.close();
*/

define(['Backbone', 'legacy_code'], function() {

window.LoadingDialog = {
    open: function (args) { 
      var dialogArgs = args || {};
      var src = window.cdnbaseurl + '/img/wait30trans.gif';
      var spinner = $("<img style='margin:30px'/>").attr('src', src);
      console.log("spinner is", spinner);
      return ScreenBlockingDialog.open(_.extend(dialogArgs, {content: spinner}));
    },
    close : function() {
      return ScreenBlockingDialog.close();
    }
};

});
