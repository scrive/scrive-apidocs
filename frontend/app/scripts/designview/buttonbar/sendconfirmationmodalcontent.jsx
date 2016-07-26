var React = require("react");
var _ = require("underscore");

var Document = require("../../../js/documents.js").Document;
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

var SendConfirmationModalSignatoriesList = React.createClass({
  propTypes: {
    signatories: React.PropTypes.array.isRequired
  },
  render: function () {
    var signatories = _.filter(this.props.signatories, function (signatory) {
      return signatory.signs() && !signatory.current();
    });

    return (
      <HtmlTextWithSubstitution
        secureText={localization.designview.startSigningModalTextWithSendout}
        lists={{
          ".put-names-of-parties-here": {
            wrapper: "<strong/>",
            items: _.map(signatories, function (item, index) {
              return item.nameForLists();
            })
          }
        }}
      />
    );
  }
});

module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    otherSignatoriesSignInPerson: React.PropTypes.bool.isRequired
  },
  render: function () {
    var content = null;

    if (this.props.otherSignatoriesSignInPerson) {
      content = localization.designview.startSigningModalTextWithPad;
    } else if (this.props.document.authorIsOnlySignatory()) {
      content = localization.designview.startSigningModalText;
    } else {
      content = React.createElement(
        SendConfirmationModalSignatoriesList,
        {signatories: this.props.document.signatories()}
      );
    }

    return (
      <p className="designview-send-confirmation-modal">
        <span>{content}</span>
      </p>
    );
  }
});
