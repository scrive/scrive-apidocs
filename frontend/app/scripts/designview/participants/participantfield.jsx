/** @jsx React.DOM */

define(["legacy_code", "React", "common/infotextinput"], function (_Legacy, React, InfoTextInput) {

return React.createClass({
  placeholderText: function () {
    var field = this.props.model;
    if (field.isMobile()) {
      return localization.phonePlaceholder;
    } else {
      return field.nicename() || field.name();
    }
  },
  render: function () {
    var self = this;
    var field = this.props.model;
    var name = field.name();
    var value = field.value();
    var csvfield = field.isCsvField();
    var csvname = self.placeholderText() + " (" + localization.designview.fromCSV + ")";
    return (
      <div className="design-view-action-participant-details-information-field-wrapper">
        <InfoTextInput
          ref="input"
          className={"design-view-action-participant-details-information-field s-input-" + name + " " +
                     (csvfield || field.isAuthorUnchangeableField() ? " transparent " : "")
                     + (!field.isValid(true) ? "redborder" : "")
          }
          infotext={csvfield ? csvname : self.placeholderText()}
          readonly={csvfield || field.isAuthorUnchangeableField()}
          value={value}
          onChange={function (val) {
            field.setValue(val.trim());
          }}
          onRemove={(
            !field.canBeRemoved() ?
              undefined :
              function () {
                mixpanel.track("Click remove field", {
                  Type: field.type(),
                  Name: field.name()
                });
                field.removeAllPlacements();
                field.signatory().deleteField(field);
              }
          )}
        />
      </div>
    );
  }
});

});
