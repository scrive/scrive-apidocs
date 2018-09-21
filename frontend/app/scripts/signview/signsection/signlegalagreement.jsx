import React from "react";

module.exports = React.createClass({
  render: function () {
    return (
      <div className="legal-agreement">
        <p className="thick">{localization.signviewLegalAgreement.textTop}</p>
        <ul>
          <li>{localization.signviewLegalAgreement.point1}</li>
          <li>{localization.signviewLegalAgreement.point2}</li>
          <li>{localization.signviewLegalAgreement.point3}</li>
        </ul>
        <p className="thick">{localization.signviewLegalAgreement.textBottom}</p>
      </div>
    );
  }
});
