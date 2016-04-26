var $ = require("jquery");



module.exports = {
  statusText :function(status) {
    if (status === "problem" ) {
      return localization.statusToolTip.problem;
    } else  if (status === "draft") {
      return localization.statusToolTip.draft;
    } else if (status === "signed") {
      return localization.statusToolTip.signed;
    } else if (status === "cancelled") {
      return localization.statusToolTip.cancelled;
    } else if (status === "timeouted") {
      return localization.statusToolTip.timeouted;
    } else if (status === "rejected") {
      return localization.statusToolTip.rejected;
    } else if (status === "opened") {
      return localization.statusToolTip.opened;
    } else if (status === "read") {
      return localization.statusToolTip.read;
    } else if (status === "deliveryproblem") {
      return localization.statusToolTip.deliveryproblem;
    } else if (status === "delivered") {
      return localization.statusToolTip.delivered;
    } else if (status === "sent") {
      return localization.statusToolTip.sent;
    } else if (status === "delivered") {
      return localization.statusToolTip.delivered;
    } else if (status === "template") {
      return localization.statusToolTip.template;
    }
  },

  showToolTip: function (status, e) {
     $('.tooltip-container').remove();
     var body = $("<div class='tooltip-body'/>");
     body.append("<div id='tooltip-"+status+"'> <div class='icon status "+status+"'></div><p>" + this.statusText(status) + "</p></div>");
     var arrow = $("<div class='tooltip-arrow'/>");
     var tooltip = $("<div class='tooltip-container'/>");
     tooltip.append(arrow);
     tooltip.append(body);
     tooltip.css({
       left: $(e.target).offset().left + $(e.target).width() + 19,
       top: $(e.target).offset().top + Math.floor($(e.target).height()/2) - 28
     });
     $('body').append(tooltip);
  },

  hideToolTip: function () {
     $('.tooltip-container').remove();
  }
};
