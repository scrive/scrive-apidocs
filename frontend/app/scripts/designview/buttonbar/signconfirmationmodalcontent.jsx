var React = require("react");

var Document = require("../../../js/documents.js").Document;
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  render: function () {
    var content = "<p></p>";
    var subs = {
      ".put-document-title-here": this.props.document.title(),
      ".put-signatory-name-here": this.props.document.author().name()
    };

    if (this.props.document.authorIsOnlySignatory()) {
      if (this.props.document.author().name().trim()) {
        content = localization.designview.signModalContentAuthorOnlyWithName;
      } else {
        content = localization.designview.signModalContentAuthorOnly;
      }
    } else {
      if (this.props.document.author().name().trim()) {
        content = localization.designview.signModalContentWithName;
      } else {
        content = localization.designview.signModalContent;
      }
    }

    return (
      <HtmlTextWithSubstitution
        className="designview-sign-confirmation-modal"
        secureText={content}
        subs={subs}
      />
    );
  }
});
