define(["signview/constants", "common/match_media_query"], function (cons, matchMediaQuery) {
  return function () {
    return matchMediaQuery("(min-width: " + cons.SIGNVIEW_SMALL_VIEW + ") and (max-width: " + cons.SIGNVIEW_MEDIUM_VIEW + ")");
  };
});
