/** @jsx React.DOM */

define(["legacy_code", "React", "common/button", "designview/participants/participantnamefield",
        "designview/participants/participantfield", "designview/participants/participantselectfield",
        "designview/participants/participantnotnamedfield", "designview/participants/participantaddfield"],
function (_Legacy, React, Button, ParticipantNameField,
          ParticipantField, ParticipantSelectField,
          ParticipantNotNamedField, ParticipantAddField) {

return React.createClass({

  render: function () {
    var self = this;
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;
    var lastTextFieldIndex = _.findLastIndex(sig.fields(), function (f) {
      return f.isText();
    });
    var lastTextField = sig.fields()[lastTextFieldIndex];

    return (
      <div className="design-view-action-participant-details-information">
        <ParticipantNameField
           last={lastTextField.isFstName() || lastTextField.isSndName()}
           model={sig}
        />
        {/* if */ sig.emailField() != undefined &&
          <ParticipantField last={lastTextField.isEmail()} model={sig.emailField()}/>
        }
        {
          _.map(sig.fields(), function (f, i) {
            if (f.isBlank()) {
              return (<ParticipantSelectField model={f} key={"select-field" + f.cid}/>);
            } else if (f.noName()) {
              return (<ParticipantNotNamedField
                         last={i == lastTextFieldIndex}
                         model={f}
                         key={"not-named-field-" + f.cid}
                      />);
            } else if (!f.isEmail() && !f.isFstName() && !f.isSndName() && f.isText()) {
              return (<ParticipantField
                         last={i == lastTextFieldIndex}
                         model={f}
                         key={"field-" + f.cid}
                      />);
            } else {
              return;
            }
          })
        }

        {/* if */ sig.isCsv() &&
          <div className="design-view-action-participant-details-information-field-wrapper">
            <Button
              ref="view-csv-button"
              text={localization.designview.viewCSV}
              type="optional"
              onClick={function () {
                mixpanel.track("Open CSV Popup");
                new CsvSignatoryDesignPopup({
                  designview: viewmodel
                });
              }}
            />
          </div>
       }

       <ParticipantAddField model={sig}/>

      </div>
    );
  }
});

});
