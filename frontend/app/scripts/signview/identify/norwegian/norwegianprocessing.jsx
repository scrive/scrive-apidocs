define(["legacy_code", "Underscore", "Backbone", "React","signview/identify/norwegian/norwegianidentifymodel"],
function (legacy_code, _, Backbone, React, NorwegianIdentifyModel) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(NorwegianIdentifyModel).isRequired
    },
    onError : function(e) {
      if (e.data == "errorPageLoaded" && e.origin == window.location.origin) {
        new FlashMessage({
          type: "error",
          content: localization.identifyBankIdNoFailed
        });
        this.props.model.setIdentify();
      }
    },
    componentDidMount: function() {
      window.addEventListener("message", this.onError);
    },
    render: function () {
      return (
        <span>
          <iframe ref="iframe" style={{minHeight: "280px", width: "100%"}} src={this.props.model.noBankIDLink()}/>
        </span>
      );
    }
  });
});
