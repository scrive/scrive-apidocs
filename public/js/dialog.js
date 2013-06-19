/* Screen blocking dialog
 *
 * Open with ScreenBlockingDialog.open({'message': some html,
 *                                      'submessage': some html,
 *                                      'withSpinner': true/false});
 * Close with ScreenBlockingDialog.close();
*/

(function(){

window.ScreenBlockingDialog = {
    dialog : function(cfg) {
        ScreenBlockingDialog.close();
        cfg.defaultSubMessage = cfg.defaultSubMessage || '';
        var spinner = cfg.withSpinner ? "<img src='/img/wait30trans.gif' style='margin:30px'/>" : "";
        var dialog = $('.screenblockingdialog') ;
        var dialog = $("<div class='modal screenblockingdialog'><div class='modal-container'><div class='modal-body'><div class='modal-content'><div class='body'>" +
                             "<center>" +
                                "<h4 class='screenblockingmessage'>" + cfg.defaultMessage + "</h4>" +
                                "<h5 class='screenblockingsubmessage'>" + cfg.defaultSubMessage + "</h6>" +
                                spinner +
                             "</center>" +
                        "</div></div></div></div></div>");
        $('body').append(dialog);
        return dialog;
    },
    changeMessage : function(message) {
      if (typeof message == 'string') {
        $(".screenblockingmessage").text(message);
      } else {
        // message must be jquery object
        $(".screenblockingmessage").empty().append(message);
      }
      ScreenBlockingDialog.fixDimensions();
    },
    changeSubMessage : function(message) {
      if (typeof message == 'string') {
        $(".screenblockingsubmessage").text(message);
      } else {
        // message must be jquery object
        $(".screenblockingsubmessage").empty().append(message);
      }
      ScreenBlockingDialog.fixDimensions();
    },
    getDialog: function() {
      return $('.modal.screenblockingdialog');
    },
    fixDimensions: function() {
      var dialog = ScreenBlockingDialog.getDialog();

      $(".modal-container",dialog).css("top",$(window).scrollTop());
      $(".modal-container",dialog).css("margin-top",($(window).height()- 200) /2);
      $(".modal-container",dialog).css("left",$(window).scrollLeft());
      $(".modal-container",dialog).css("margin-left",($(window).width() - 650) / 2);
    },
    open: function (cfg) {
         var dialog = ScreenBlockingDialog.dialog(cfg);
         dialog.css('display','block');
         $(".screenblockingmessage", dialog).html(cfg.message);
         $(".screenblockingsubmessage", dialog).html(cfg.submessage);
         ScreenBlockingDialog.fixDimensions();
         dialog.addClass('active');
         return dialog;
    },
    close : function() {
       var dialog =  $('.screenblockingdialog');
       if (dialog.size() > 0 ) {
         dialog.removeClass("active");
         if (BrowserInfo.isIE9orLower())
           dialog.css('display','none');
         dialog.remove();
      }
    }
};
})();

