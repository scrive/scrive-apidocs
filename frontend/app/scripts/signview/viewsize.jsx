var vars = require("../../less/signview/vars.less");
var matchMediaQuery = require("../common/match_media_query");

  module.exports = {

    isSmall: function () {
      return matchMediaQuery("not all and (min-width: " + vars.signviewSmallView + "px)");
    },

    isMedium: function () {
      var query = "(min-width: " + vars.signviewSmallView + "px) and (max-width: " + vars.signviewMediumView + "px)";
      return matchMediaQuery(query);
    }
  };
