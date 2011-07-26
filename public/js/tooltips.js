/* Basic buttons
 * Usage
 *  ToolTip.set({
 *              on: jQuery | on what to set the tooltip
 *              tip: jQuery | text to be put there
 *              });
*/
  
window.ToolTip= {
    set: function (args) {
           var body = $("<div class='tooltip-body'/>");
           body.append(args.tip);
           var arrow = $("<div class='tooltip-arrow'/>");
           var container = $("<div class='tooltip-container'/>");
           container.append(arrow);
           container.append(body);
            $(args.on).mouseenter(function(e) {
                container.appendTo('body');
                container.css({
                    left: $(this).offset().left + $(this).width() + 19,
                    top: $(this).offset().top - 20
                });
             })
            $(args.on).mouseleave(function() {
                container.remove();
             });
        }
    
}


