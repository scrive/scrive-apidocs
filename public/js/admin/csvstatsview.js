/* The static CSV view. Implemented as a Backbone view to enable future
 * extensions.
 */
(function(window){

var CSVStatsView = Backbone.View.extend({
        links: [
            {
                url: "/adminonly/allstatscsv",
                text: "Download the CSV containing all signature stats"
            },
            {
                url: "/adminonly/userstatscsv",
                text: "Download the CSV containing all user stats"
            },
            {
                url: "/adminonly/signstatscsv",
                text: "Download the CSV containing all sign stats"
            },
            {
                url: "/adminonly/dochistorycsv",
                text: "Download the CSV containing document histories"
            },
            {
                url: "/adminonly/signhistorycsv",
                text: "Download the CSV containing sign histories"
            },
            {
                url: "/adminonly/userslistcsv",
                text: "Download the CSV containing all users (not suspended, with accepted TOS, no integration parties)"
            },
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
