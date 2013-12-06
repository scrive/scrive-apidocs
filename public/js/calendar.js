(function(window){

window.Calendar = Backbone.Model.extend({
    defaults: {
        on : $('<div/>'),
        change : function() {return false},
        value : 0,
        maxValue : 90
    },
    initialize : function(args){
        var activator  = this.get("on");
        var onchange = this.get("change");
        activator.dateinput({
            format: 'dd-mm-yy',
            value : args.days == undefined ? undefined : new Date(new Date().getTime() + args.days * 24 * 60 * 60 * 1000),
            change: function() {
                var dist = activator.data("dateinput").getValue().diffDays() + 1;
                onchange(dist);
            },
            min: 0,
            max: this.get("maxValue"),
            onShow : function(a,b,c) {
              $("#calroot").css("top",activator.offset().top);
            }
        });
    },
    setDays : function(days) {
            if (days != undefined && !isNaN(days)) {
              var date = new Date();
              date.setDate(date.getDate() + days);
              this.get("on").data("dateinput").setValue(date);
            }
    },
    setMax : function(days) {
            if (days != undefined && !isNaN(days)) {
              this.get("on").data("dateinput").setMax(days);
            }
    },
    close : function() {
            this.get("on").data("dateinput").hide();
    }
});





})(window);
