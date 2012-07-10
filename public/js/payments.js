/*

  Payments 



*/

(function(window) {

    var PaymentsModel = Backbone.Model.extend({
        
    });

    window.bootPaymentsDashboard = function(selector) {
        console.log("pd");
        $(function() {
            var el = $(selector);
            el.append("hello!");
        });
    };

})(window);
