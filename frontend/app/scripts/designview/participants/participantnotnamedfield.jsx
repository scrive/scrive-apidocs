var React = require("react");
var InfoTextInput = require("../../common/infotextinput");
var Button = require("../../common/button");
var Track = require("../../common/track");
var _ = require("underscore");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

module.exports = React.createClass({
  sameNameExists: function () {
    var self = this;
    var field =  this.props.model;
    var customFields = this.props.model.signatory().customFields();
    return _.any(customFields, function (c) { return self.state.name == c.name() && c != field; });
  },
  getInitialState: function () {
    return {name: ""};
  },
  render: function () {
    var self = this;
    var field = this.props.model;
    var sig = field.signatory();
    return (
      <div className="design-view-action-participant-details-information-field-wrapper">

        <InfoTextInput
          ref="input"
          className={
            "design-view-action-participant-new-field-name-input redborder " +
            (self.sameNameExists() ? "conflict" : "")
          }
          infotext={localization.designview.fieldName}
          value={self.state.name}
          onChange={function (val) {
            self.setState({name: val});
          }}
          onEnter={function () {
            if (self.sameNameExists()) {
              new FlashMessage({type: "error", content: localization.designview.fieldWithSameNameExists});
              return;
            }
            Track.track("Enter custom field name", {
              "Field name": self.state.name
            });
            field.setName(self.state.name);
          }}
          onTab={function (e) { if (self.props.last) { e.preventDefault(); } }}
          onRemove={function () {
            Track.track("Click remove field", {
              Type: field.type(),
              Name: field.name()
            });
            field.removeAllPlacements();
            sig.deleteField(field);
          }}
        />

        <Button
          ref="button"
          text={localization.ok}
          width={44}
          onClick={function () {
            if (self.sameNameExists()) {
              new FlashMessage({type: "error", content: localization.designview.fieldWithSameNameExists});
              return;
            }
            Track.track("Enter custom field name", {
              "Field name": self.state.name
            });
            field.setName(self.state.name);
          }}
        />

      </div>
    );
  }
});
