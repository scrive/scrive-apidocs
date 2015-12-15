define(["signview/constants", "common/match_media_query"], function (cons, matchMediaQuery) {
  return function () {
    return matchMediaQuery("not all and (min-width: " + cons.SIGNVIEW_SMALL_VIEW + ")");
  };
});
