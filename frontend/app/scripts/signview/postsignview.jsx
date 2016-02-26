var React = require("react");
var Backbone = require("backbone");
var CreateAccountView = require("./createaccount/createaccountview");
var FeedbackView = require("./feedback/feedbackview");
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;

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
      return null !== /^(backbone|loxysoft|caspeco).scrive.com/.exec(location.host);
    },
    isQuestionaire : function() {
      return (!this.isNotInterestedInQuestionnaire() && this.props.document.currentSignatory().company() !== '');
    },
    isSaveCopy : function() {
      return !this.isHidden() && !this.isQuestionaire();
    },
    componentWillMount : function() {
      var promotionName = "";
      if (this.isQuestionaire()) {
        promotionName = "Questionnaire";
      }
      mixpanel.track("Store copy button shown", promotionName ? { promo: promotionName } : {});
    },
    render: function() {
      var document = this.props.document;
      if(this.isHidden()) {
        return (<div/>);
      } else if(this.isQuestionaire()) {
        return <FeedbackView document={document} />;
      } else if (this.isSaveCopy()) {
        return (<CreateAccountView document={document}/>);
      }
    }
  });
