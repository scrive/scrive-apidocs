/* Promo modal
 *
 * Open with Promo.open(some html);
 * Change with Promo.change(some html);
 * Close with Promo.close();
*/

(function(){

window.Promo = {
    modal: function() {
        var modal = $('.promomodal') ;
        if (modal.size() == 0)
        {
          var modal = $("<div class='modal promomodal'><div class='modal-container'><div class='modal-body'><div class='modal-content'><div class='body'></div></div></div></div></div>");
          $('body').append(modal);
        }
        return modal;
    },
    change: function(message) {
         $(".promomodal").text(message);
    },
    open: function (message) {
         var modal = Promo.modal();
         modal.css('display', 'block'); // because of IE<=8 issues with hiding
         $(".body", modal).html(message);
         $(".modal-container",modal).css("top",$(window).scrollTop());
         $(".modal-container",modal).css("margin-top",($(window).height()- 200) /2);
         $(".modal-container",modal).css("left",$(window).scrollLeft());
         $(".modal-container",modal).css("margin-left",($(window).width() - 800) / 2);

         modal.addClass('active');
    },
    close: function() {
       var modal =  $('.promomodal');
       if (modal.size() > 0 ) {
         modal.removeClass("active");
         if (BrowserInfo.isIE8orLower())
           modal.css('display','none');
      }
    }
};
})();

