var React = require("react");
var _ = require("underscore");
var classNames = require("classnames");
var ConsentModule = require("../../../js/consentmodule").ConsentModule;
var ConsentQuestionView = require("./consentquestionview");
var BrowserInfo = require("../../../js/utils/browserinfo.js").BrowserInfo;

module.exports = React.createClass({
  displayName: "ConsentModuleView",

  propTypes: {
    model: React.PropTypes.instanceOf(ConsentModule).isRequired
  },

  render: function() {
    var model = this.props.model;

    var sectionClass = classNames("section consent-module", {
      "small-screen": BrowserInfo.isSmallScreen()
    });

    var questions = _.map(model.questions(), function(question) {
      return (
        <ConsentQuestionView model={question} />
      );
    });

    return (
      <div className={sectionClass}>
        <h1 className="title">{model.title()}</h1>
        {questions}
      </div>
    );
  }
});
