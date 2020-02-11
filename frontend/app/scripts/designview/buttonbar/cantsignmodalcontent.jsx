var React = require("react");

var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");

module.exports = React.createClass({
  render: function () {
    return (
      <div className="designview-cant-sign-modal">
        <p className="paragraph">
          <HtmlTextWithSubstitution
            secureText={localization.designview.cantSignModal.info1}
          />
        </p>
        <p className="paragraph">
          <HtmlTextWithSubstitution
            secureText={localization.designview.cantSignModal.info2}
          />
        </p>

        <ul className="unordered-list">
          <li>{localization.designview.cantSignModal.li1}</li>
          <li>{localization.designview.cantSignModal.li2}</li>
          <li>{localization.designview.cantSignModal.li3}</li>
          <li>{localization.designview.cantSignModal.li4}</li>
        </ul>
      </div>
    );
  }
});
