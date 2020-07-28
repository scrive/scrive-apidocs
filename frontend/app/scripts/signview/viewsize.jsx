import {toLessInteropLoader} from '../common/less_utils.jsx';
import vars_ from '!@hon2a/less-vars-loader!../../less/signview/vars.less';
const vars = toLessInteropLoader(vars_);
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
