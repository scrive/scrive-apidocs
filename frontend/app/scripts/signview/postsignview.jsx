var React = require("react");
var Backbone = require("backbone");
var FeedbackView = require("./feedback/feedbackview");
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var Track = require("../common/track");

module.exports = React.createClass({
    propTypes: {
      document : React.PropTypes.object.isRequired
    },
    isHidden : function() {
      if (null !== /^avis.*scrive.com/.exec(location.host))
        return true;
      if (null !== /^budget.*scrive.com/.exec(location.host))
        return true;
    },
    isNotInterestedInQuestionnaire: function() {
      // We need to tell this environment somehow that this is a branded domain. This was a quicky fix for LoxySoft and has to be reworked.
      return null !== /^(backbone|loxysoft|caspeco|api-testbed).scrive.com/.exec(location.host);
    },
    isQuestionaire : function() {
      return !this.isNotInterestedInQuestionnaire() && !this.isHidden();
    },
    componentWillMount : function() {
      var promotionName = "";
      if (this.isQuestionaire()) {
        promotionName = "Questionnaire";
      }
      Track.track("Store copy button shown", promotionName ? { promo: promotionName } : {});
    },
    render: function() {
      var document = this.props.document;
      if(this.isQuestionaire()) {
        return <FeedbackView document={document} />;
      } else {
        return (<div/>);
      }
    }
  });
