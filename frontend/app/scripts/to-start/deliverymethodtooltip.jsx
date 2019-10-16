var $ = require("jquery");
var capitaliseFirstLetter = require("../common/capitalise_first_letter");



module.exports = {
  getDeliveryMethodText: function(dm) {
      if (dm == "email") {
        return capitaliseFirstLetter(localization.delivery.email);
      } else if (dm == "pad") {
        return capitaliseFirstLetter(localization.delivery.pad);
      } else if (dm == "mobile") {
        return capitaliseFirstLetter(localization.delivery.mobile);
      } else if (dm == "email_mobile") {
        return capitaliseFirstLetter(localization.delivery.email_mobile);
      } else if (dm == "api") {
        return capitaliseFirstLetter(localization.delivery.link);
      } else if (dm == "portal") {
        return capitaliseFirstLetter(localization.delivery.portal);
      } else {
        return "";
      }
  },
  dmIcons: {
      email: 'design-view-action-participant-icon-device-icon-email',
      pad: 'design-view-action-participant-icon-device-icon-pad',
      api: 'design-view-action-participant-icon-device-icon-pad',
      mobile: 'design-view-action-participant-icon-device-icon-phone',
      email_mobile : 'design-view-action-participant-icon-device-icon-email-mobile',
      portal : 'design-view-action-participant-icon-device-icon-portal'
  },
  showToolTip : function(deliverymethod,e) {
     $('.tooltip-container').remove();
     var body = $("<div class='tooltip-body'/>");
     body.append("<div id='tooltip'> <div class='icon "+this.dmIcons[deliverymethod]+"'></div><p>"+ this.getDeliveryMethodText(deliverymethod)+"</p></div>");
     var arrow = $("<div class='tooltip-arrow'/>");
     var tooltip = $("<div class='tooltip-container to-start'/>");
     tooltip.append(arrow);
     tooltip.append(body);

     var tooltipWidth = 209;
     var tooltipMargin = 19;

     tooltip.css({
       left: $(e.target).offset().left - $(e.target).width() - tooltipWidth + tooltipMargin,
       top: $(e.target).offset().top + Math.floor($(e.target).height()/2) - 28
     });
     $('body').append(tooltip);
  },
  hideToolTip : function() {
     $('.tooltip-container').remove();
  }
};
