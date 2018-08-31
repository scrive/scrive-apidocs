import React from "react";

module.exports = React.createClass({
  render: function () {
    return (
      <div className="legal-agreement">
        <p>{localization.signviewLegalAgreement.text}</p>
        <ul>
          <li>{localization.signviewLegalAgreement.point1}</li>
          <li>{localization.signviewLegalAgreement.point2}</li>
          <li>{localization.signviewLegalAgreement.point3}</li>
        </ul>
      </div>
    );
  }
});
