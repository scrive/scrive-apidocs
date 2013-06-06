/* The static CSV view. Implemented as a Backbone view to enable future
 * extensions.
 */
(function(window){

var CSVStatsView = Backbone.View.extend({
        links: [
            {
                url: "/adminonly/paymentsstats.csv",
                text: "Download the CSV containing payments stats"
            }
        ],

        initialize: function() {
            this.render();
        },

        el: $("<div class='tab-container' style='padding: 30px;background: none repeat scroll 0 0 #FFFFFF;'/>"),

        render: function() {
            var el = $(this.el);
            _.each(this.links, function(link) {
                el.append('<p><a href="' + link.url + '">' +
                    link.text + '</a></p>');
            });
        }

    });
window.CSVStats = function(args) {
          var view =  new CSVStatsView({});
          return new Object({
              el  : function() {return $(view.el);}
            });
};
})(window);
