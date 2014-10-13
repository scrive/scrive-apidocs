define(['Backbone', 'common/language_service', 'legacy_code'], function(Backbone, LanguageService) {

(function() {
  var moment_lang_data = LanguageService.momentLangDataForCurrentLanguage();
  var join_and_capitalize = function(strings) {
    return (_.map(strings, function (s) { return s.charAt(0).toUpperCase() + s.slice(1);})).join(',');
  };
  $.tools.dateinput.localize(LanguageService.currentLanguage(),
                             {months: join_and_capitalize(moment_lang_data._months),
                              shortMonths: join_and_capitalize(moment_lang_data._monthsShort),
                              days: join_and_capitalize(moment_lang_data._weekdays),
                              shortDays: join_and_capitalize(moment_lang_data._weekdaysShort)});
})();

window.Calendar = Backbone.Model.extend({
    defaults: {
        on : $('<div/>'),
        change : function() {return false},
        changing: false,
        maxValue : 90
    },
    initialize : function(args){
        var self = this;
        var activator  = this.get("on");
        var onchange = this.get("change");
        activator.dateinput({
            format: 'dd-mm-yy',
            lang: LanguageService.currentLanguage(),
            value : args.days == undefined ? undefined : new Date(new Date().getTime() + args.days * 24 * 60 * 60 * 1000),
            change: function() {
                if (self.get('changing')) {
                  return;
                }
                var dist = activator.data("dateinput").getValue().diffDays();
                if (activator.data("dateinput").getValue() <= new Date())
                  dist = undefined;
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
            this.set('changing', true);
            if (days != undefined && !isNaN(days)) {
              var date = new Date();
              date.setDate(date.getDate() + days);
              this.get("on").data("dateinput").setValue(date);
            } else
              this.get("on").data("dateinput").setValue(new Date());
            this.set('changing', false);
    },
    setMax : function(days) {
            this.set('changing', true);
            if (days != undefined && !isNaN(days)) {
              this.get("on").data("dateinput").setMax(days);
            }
            this.set('changing', false);
    },
    close : function() {
            this.get("on").data("dateinput").hide();
    }
});

});
