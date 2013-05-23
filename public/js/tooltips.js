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
           if (args.theme != undefined)
             container.addClass(args.theme + "-theme")
           container.append(arrow);
           container.append(body);
            $(args.on).mouseenter(function(e) {
                var checker = function() {
                    if ($(':hover',args.on).size() == 0)
                    {
                        container.remove();
                    }
                    else
                        setTimeout(checker,1000);
                }
                setTimeout(checker,1000);
                container.appendTo('body');
                container.css({
                    left: $(this).offset().left + $(this).width() + 19,
                    top: $(this).offset().top + Math.floor($(args.on).height()/2) - 28
                });
             });
            $(args.on).mouseleave(function() {
                container.remove();
             });
        }

};


