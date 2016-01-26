define(["signview/constants", "common/match_media_query"], function (cons, matchMediaQuery) {
  return {

    isSmall: function () {
      return matchMediaQuery("not all and (min-width: " + cons.SIGNVIEW_SMALL_VIEW + ")");
    },

    isMedium: function () {
      var query = "(min-width: " + cons.SIGNVIEW_SMALL_VIEW + ") and (max-width: " + cons.SIGNVIEW_MEDIUM_VIEW + ")";
      return matchMediaQuery(query);
    }
  };
});
