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
      return ["en","sv","de","fr","it","es","pt","nl","da","no","el","fi","is","et","lv","lt","cs","pl","hu"];
    },
    allLanguagesOptions : function() {
      return [
          {name: localization.languages.enInEn, value: "en"},
          {name: localization.languages.svInSv, value: "sv"},
          {name: localization.languages.deInDe, value: "de"},
          {name: localization.languages.frInFr, value: "fr"},
          {name: localization.languages.itInIt, value: "it"},
          {name: localization.languages.esInEs, value: "es"},
          {name: localization.languages.ptInPt, value: "pt"},
          {name: localization.languages.nlInNl, value: "nl"},
          {name: localization.languages.daInDa, value: "da"},
          {name: localization.languages.noInNo, value: "no"},
          {name: localization.languages.elInEl, value: "el"},
          {name: localization.languages.fiInFi, value: "fi"},
          {name: localization.languages.isInIs, value: "is"},
          {name: localization.languages.etInEt, value: "et"},
          {name: localization.languages.lvInLv, value: "lv"},
          {name: localization.languages.ltInLt, value: "lt"},
          {name: localization.languages.csInCs, value: "cs"},
          {name: localization.languages.plInPl, value: "pl"},
          {name: localization.languages.huInHu, value: "hu"}
        ];
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

