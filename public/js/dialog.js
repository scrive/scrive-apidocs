/* Screen blocking dialog
 *
 * Open with ScreenBlockingDialog.open({'header': some html/jquery object,
 *                                      'subheader': some html/jquery object,
 *                                      'content': some html/jquery object});
 * Close with ScreenBlockingDialog.close();
*/

(function(){

var fillWith = function(e, s) {
  e.children().detach();
  e.empty();
  if (typeof s == 'string') {
    e.text(s);
  } else {
    e.append(s);
  }
};

var buildDialog = function(cfg) {
  var dialog = $('<div class="modal screenblockingdialog" />');
  if (BrowserInfo.isPadDevice()) {
            //Pad devices have a different aproach to body.width
            //Check http://stackoverflow.com/questions/6695676/100-width-css-issue-only-on-mobile-safari
            // Note also that width of sign page is dynamic due to arrows
            dialog.css("min-width",$(document).width());
  }
  dialog.height($(document).height());
  var modalcontainer = $('<div class="modal-container" />');
  dialog.append(modalcontainer);
  var modalbody = $('<div class="modal-body" />');
  modalcontainer.append(modalbody);
  var modalcontent = $('<div class="modal-content" />');
  modalbody.append(modalcontent);
  var body = $('<div class="body" />');
  modalcontent.append(body);
  var center = $('<center />');
  body.append(center);

  var header = $('<h4 class="screenblockingheader" />');
  fillWith(header, cfg.header);
  center.append(header);

  var subheader = $('<h4 class="screenblockingsubheader" />');
  fillWith(subheader, cfg.subheader);
  center.append(subheader);

  var content = $('<div class="screenblockingcontent" />');
  fillWith(content, cfg.content);
  center.append(content);

  $('body').append(dialog);
  return dialog;
};

window.ScreenBlockingDialog = {
    dialog : function(cfg) {
      var dialog = $('.modal.screenblockingdialog');
      if (dialog.size() == 0) {
        return buildDialog(cfg);
      } else {
        return dialog;
      }
    },
    fixposition : function(dialog) {
      var modalcontainer = $('.modal-container', dialog);
      modalcontainer.css('top', $(window).scrollTop());
      modalcontainer.css('margin-top', ($(window).height()- 200) /2);
      modalcontainer.css('left', $(window).scrollLeft());
      modalcontainer.css('margin-left', ($(window).width() - 650) / 2);
      dialog.height($(document).height());
    },
    open: function (cfg) {
      var dialog = ScreenBlockingDialog.dialog(cfg);
      this.fixposition(dialog);
      var header = $('.screenblockingheader', dialog);
      fillWith(header, cfg.header);
      var subheader = $('.screenblockingsubheader', dialog);
      fillWith(subheader, cfg.subheader);
      var content = $('.screenblockingcontent', dialog);
      fillWith(content, cfg.content);
      dialog.css('display','block');
      dialog.addClass('active');
      return dialog;
    },
    close : function() {
      var dialog = $('.screenblockingdialog');
      if (dialog.size() > 0 ) {
        dialog.removeClass("active");
        if (BrowserInfo.isIE())
          dialog.css('display','none');
        else // We need to hide this dialog, else it will cause screen to expand if size changed.
          setTimeout(function() { if (!dialog.hasClass('active')) dialog.css('display','none');}, 500);
      }
    }
};
})();
