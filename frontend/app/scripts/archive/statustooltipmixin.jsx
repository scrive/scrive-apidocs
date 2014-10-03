/** @jsx React.DOM */


define(['Backbone', 'legacy_code'], function() {

return {
  showToolTip : function(status,e) {
     $('.tooltip-container').remove();
     var body = $("<div class='tooltip-body'/>");
     body.append("<div id='tooltip-"+status+"'> <div class='icon status "+status+"'></div><p>"+ localization.statusToolTip[status]+"</p></div>");
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
  hideToolTip : function() {
     $('.tooltip-container').remove();
  }
};



});
