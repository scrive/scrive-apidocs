/* Loading dialog
 *
 * Open with LoadingDialog.open(some html);
 * Close with LoadingDialog.close();
*/

(function(){

window.LoadingDialog = {
    dialog : function() {
        var dialog = $('.loadingdialog') ;
        if (dialog.size() == 0)
        {
          var dialog = $("<div class='modal loadingdialog'><div class='modal-container'><div class='modal-body'><div class='modal-content'><div class='body'>" +
                             "<center>" +
                                "<h4 class='loadingmessage'>Loading</h4>" +
                                "<h5 class='loadingsubmessage' style='font-size: 18px; color: #808080;'></h5>" +
                                     "<img src='/img/wait30trans.gif' style='margin:30px'/>" +
                             "</center>" +
                        "</div></div></div></div></div>");
          $('body').append(dialog);
        }
        return dialog;
    },
    changeMessage : function(message) {
         $(".loadingmessage").text(message);
    },
    open: function (message,submessage) {
         var dialog = LoadingDialog.dialog();
         dialog.css('display','block');
         $(".loadingmessage", dialog).html(message);
         $(".loadingsubmessage", dialog).html(submessage || "");
         $(".modal-container",dialog).css("top",$(window).scrollTop());
         $(".modal-container",dialog).css("margin-top",($(window).height()- 200) /2);
         $(".modal-container",dialog).css("left",$(window).scrollLeft());
         $(".modal-container",dialog).css("margin-left",($(window).width() - 600) / 2);

         dialog.addClass('active');
    },
    close : function() {
       var dialog =  $('.loadingdialog');
       if (dialog.size() > 0 ) {
         dialog.removeClass("active");
         if (BrowserInfo.isIE9orLower())
           dialog.css('display','none');
      }
    }
};
})();

