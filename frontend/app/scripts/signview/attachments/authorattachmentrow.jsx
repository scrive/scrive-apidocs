define(["legacy_code", "Underscore", "Backbone", "React", "common/button"],
  function (legacy_code, _, Backbone, React, Button) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      canSign: React.PropTypes.bool.isRequired
    },

    render: function () {
      var attachment = this.props.model;
      var canSign = this.props.canSign;

      var buttonClass = React.addons.classSet({
        "float-right": true,
        "attachment-download-button": true,
        "for-signing": canSign
      });

      return (
        <tr>
          <td className="desc">
            <div className="item">
              <div className="icon"></div>
              <div className="label">
                <div className="name">{attachment.name()}</div>
              </div>
              <div className="clearfix"></div>
            </div>
          </td>
          <td className="file">
            <div className="item">
              <Button
                type={canSign ? "optional" : ""}
                text={localization.reviewPDF}
                className={buttonClass}
                size="small"
                onClick={function () {
                  window.open(attachment.downloadLink(), "_blank");
                }}
              />
            </div>
          </td>
        </tr>
      );
    }
  });
});
