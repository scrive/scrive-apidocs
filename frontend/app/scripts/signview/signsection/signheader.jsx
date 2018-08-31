import React from "react";
import HtmlTextWithSubstitution from "../../common/htmltextwithsubstitution";

import SignLegalAgreement from "./signlegalagreement";

module.exports = React.createClass({
  propTypes: {
    title: React.PropTypes.string.isRequired,
    name: React.PropTypes.string.isRequired,
    showLegalText: React.PropTypes.bool.isRequired
  },

  render: function () {
    return (
      <div>
        <h1>{localization.process.signbuttontext}</h1>
        {/* if */ this.props.showLegalText && <SignLegalAgreement /> }
        <p>
          <HtmlTextWithSubstitution
             subs={{".put-document-title-here": this.props.title,
                    ".put-signatory-name-here": this.props.name}}
            secureText={localization.signviewConfirmation}
          />
        </p>
      </div>
    );
  }
});
