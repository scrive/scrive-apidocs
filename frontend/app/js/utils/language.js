/* Language changing script. Language is interface for getting informations about current language and changing it.
 *
 * .current() gives you current language based on localization
 * .changeforPageAndUserAndReload changes user language (if there is one), and reloades page. If page url starts with language code (en or sv), this code will be also changed.
 */

define(['Backbone', 'legacy_code'], function() {

window.Language = {
    current : function() {
       return localization.code;
    },
    allLanguagesCodes : function() {
      return ["en","sv","de","fr","it","es","pt","nl","da","no","el","fi"];
    },
    changeForPageAndUserAndReload : function(code) {
        $.post('/api/frontend/changelanguage', {lang: code }, function() {
               Language.changeForPageAndReload(code);
        });
    },
    changeForPageAndReload : function(code) {
        var p = window.location.pathname;
        Cookies.set("lang",code);
        var hasLangPrefix = _.any(Language.allLanguagesCodes(), function(c) {return p.substring(0, 3) == ('/'+ c);});
        if (hasLangPrefix) {
            p = "/" + code + p.substr(3);
            window.location.pathname = p;
        } else
            window.location.reload();
    }
};

});
