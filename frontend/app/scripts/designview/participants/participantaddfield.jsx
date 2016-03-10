var React = require("react");
var Button = require("../../common/button");
var Field = require("../../../js/fields.js").Field;

module.exports = React.createClass({
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-new-field-selector">
        <Button
          ref="add-field-button"
          text={localization.designview.addField}
          onClick={function () {
            mixpanel.track("Click add field");
            var field = new Field({
              name: "",
              type: "",
              value: "",
              signatory: sig,
              obligatory: false,
              shouldbefilledbysender: sig.author()
            });
            if (field.obligatory() && field.shouldbefilledbysender()) {
              field.authorObligatory = "sender";
            } else if (field.obligatory()) {
              field.authorObligatory = "recipient";
            } else {
              field.authorObligatory = "optional";
            }
            sig.addField(field);
          }}
        />
      </div>
    );
  }
});
