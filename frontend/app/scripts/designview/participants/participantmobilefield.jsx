var classNames = require("classnames");
var React = require("react");

var PhoneNumberInput = require("../../common/phone_number_input");
var Track = require("../../common/track");

var ParticipantMobileField = React.createClass({
  placeholderText: function () {
    return localization.phonePlaceholder;
  },
  onInputChange: function (value) {
    this.props.model.setValue(value.trim());
  },
  onInputTab: function (event) {
    if (this.props.last) {
      event.preventDefault();
    }
  },
  remove: function () {
    Track.track("Click remove field", {
      Type: this.props.model.type(),
      Name: this.props.model.name()
    });

    this.props.model.removeAllPlacements();
    this.props.model.signatory().deleteField(this.props.model);
  },
  onInputRemove: function () {
    if (this.props.model.canBeRemoved()) {
      this.remove();
    }
  },
  render: function () {
    var field = this.props.model;
    var name = field.type();
    var value = field.value();
    var csvfield = field.isCsvField();
    var csvname = (
      this.placeholderText() + " (" + localization.designview.fromCSV + ")"
    );

    var inputClassName = classNames(
      "design-view-action-participant-details-information-field",
      "s-input-" + name,
      {
        transparent: (csvfield || field.isAuthorUnchangeableField()),
        redborder: !field.isValid(true)
      }
    );

    return (
      <div className="design-view-action-participant-details-information-field-wrapper">
        <PhoneNumberInput.PhoneNumberInput
          ref="input"
          className={inputClassName}
          infotext={csvfield ? csvname : this.placeholderText()}
          readonly={csvfield || field.isAuthorUnchangeableField()}
          disabled={csvfield || field.isAuthorUnchangeableField()}
          value={value}
          onChange={this.onInputChange}
          onRemove={this.onInputRemove}
          onTab={this.onInputTab}
        />
      </div>
    );
  }
});

module.exports = ParticipantMobileField;
