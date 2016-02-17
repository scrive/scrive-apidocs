var Cookies = require("./cookie.js").Cookies;
var _ = require("underscore");
var Language = require("./language.js").Language;

/* Language changing script. Language is interface for getting informations about current language and changing it.
 *
 * .current() gives you current language based on localization
 */


var Language = exports.Language = {
    current : function() {
       return localization.code;
    },
    allLanguagesCodes : function() {
      return ["en","sv","de","fr","it","es","pt","nl","da","no","el","fi","is"];
    },
    changeForPageAndReload : function(code) {
        var p = window.location.pathname;
        Cookies.set("lang",code);
        var hasLangPrefix = _.any(Language.allLanguagesCodes(), function(c) {return p.substring(0, 4) == ("/" + c + "/");});
        if (hasLangPrefix) {
            p = "/" + code + p.substr(3);
            window.location.pathname = p;
        } else
            window.location.reload();
    }
};

