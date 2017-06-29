var React = require("react");
var InfoTextInput = require("../../common/infotextinput");

module.exports = React.createClass({

  render: function () {
    var self = this;
    var sig = this.props.model;
    var fstnameField = sig.fstnameField();
    var sndnameField = sig.sndnameField();
    var fstnameFieldValid = (fstnameField === undefined) || fstnameField.isValid();
    var sndnameFieldValid = (sndnameField === undefined) || sndnameField.isValid();
    var nameFieldsValid = fstnameFieldValid && sndnameFieldValid;
    var csvfield = (fstnameField !== undefined) && fstnameField.isCsvField();
    var csvname = localization.designview.fullName + " (" + localization.designview.fromCSV + ")";

    return (
      <div className="design-view-action-participant-details-information-field-wrapper">
        <InfoTextInput
          ref="input"
          className={
            "design-view-action-participant-details-information-field s-input-fullname" +
            (csvfield || sig.author() ? " transparent" : "") +
            (!nameFieldsValid ? " redborder" : "")
          }
          infotext={csvfield ? csvname : localization.designview.fullName}
          readonly={csvfield || sig.author()}
          disabled={csvfield || sig.author()}
          value={sig.name()}
          onTab={function (e) { if (self.props.last) { e.preventDefault(); } }}
          onChange={function (val) {
            var str = val.trim();
            var i = str.indexOf(" ");
            var f;
            var s;
            if (i >= 0) {
              f = str.slice(0, i).trim();
              s = str.slice(i + 1).trim();
            } else {
              f = str.trim();
              s = "";
            }
            if (fstnameField === undefined && sndnameField !== undefined) {
              sndnameField.setValue(str);
            } else if (fstnameField !== undefined && sndnameField === undefined) {
              fstnameField.setValue(str);
            } else if (fstnameField !== undefined && sndnameField !== undefined) {
              fstnameField.setValue(f);
              sndnameField.setValue(s);
            }
          }}
        />
      </div>
    );
  }
});
