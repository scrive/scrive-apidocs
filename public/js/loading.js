/* Loading dialog
 * 
 * Open with LoadingDialog.open(some html);
 * Close with LoadingDialog.close();
*/
  
(function(){

window.LoadingDialog = {
    dialog : function() {
        var dialog = $('#loadingdialog') ;
        if (dialog.size() == 0)
        {
          var dialog = $("<div id='loadingdialog' class='overlay'>" +
                              "<a href='#' id='loadinglink' style='display:none;' rel='#loadingdialog'></a>" +
                                "<div class='modal-container'>" +
                                    "<div class='modal-body' style='padding:30px'>" +
                                        "<center>" +
                                            "<h3 id='loadingmessage'>Loading</h3>" +
                                            "<img src='/img/wait30trans.gif' style='margin:30px'/>" +
                                        "</center>" +
                                    "</div>" +
                                "</div>" +
                            "</div>");
          dialog.overlay({
            mask: standardDialogMask,
            top: standardDialogTop,
            resizable: false,
            closeOnClick: false,
            closeOnEsc: false,
            load: false,
            fixed:false
          }); 
          $('body').append(dialog);
        }
        return dialog;
    },
    changeMessage : function(message) {
         $("#loadingmessage").text(message);
    },
    open: function (message) {
         var dialog = LoadingDialog.dialog();
         $("#loadingmessage", dialog).html(message);
         dialog.overlay({
                fixed:false
            }).load();
    },        
    close : function() {
       var dialog =  $('#loadingdialog');
       if (dialog.size() > 0 ) dialog.overlay().close();
    }
};
})();

