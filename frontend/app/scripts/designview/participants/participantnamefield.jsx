/** @jsx React.DOM */

define(["legacy_code", "React", "common/infotextinput"], function (_Legacy, React, InfoTextInput) {

return React.createClass({

  render: function () {
    var self = this;
    var sig = this.props.model;
    var fstnameField = sig.fstnameField();
    var sndnameField = sig.sndnameField();
    var csvfield = fstnameField.isCsvField();
    var csvname = localization.designview.fullName + "(" + localization.designview.fromCSV + ")";

    return (
      <div className="design-view-action-participant-details-information-field-wrapper">
        <InfoTextInput
          ref="input"
          className={
            "design-view-action-participant-details-information-field s-input-fullname" +
            (csvfield || sig.author() ? " transparent" : "")
          }
          infotext={csvfield ? csvname : localization.designview.fullName}
          readonly={csvfield || sig.author()}
          value={sig.name()}
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
            if (sndnameField != undefined) {
              fstnameField.setValue(f);
              sndnameField.setValue(s);
            } else {
              fstnameField.setValue(str);
            }
          }}
        />
      </div>
    );
  }
});

});
