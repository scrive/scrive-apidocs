var React = require("react");
var Button = require("../../common/button");
var Track = require("../../common/track");
var ParticipantNameField = require("./participantnamefield");
var ParticipantField = require("./participantfield");
var ParticipantSelectField = require("./participantselectfield");
var ParticipantNotNamedField = require("./participantnotnamedfield");
var ParticipantAddField = require("./participantaddfield");
var _ = require("underscore");
var CsvSignatoryDesignPopup = require("../../../js/designview/csvsignatorydesign.js").CsvSignatoryDesignPopup;
var ParticipantMobileField = require("./participantmobilefield");

module.exports = React.createClass({

  render: function () {
    var self = this;
    var sig = this.props.model;
    var lastTextFieldIndex = _.findLastIndex(sig.fields(), function (f) {
      return f.isText();
    });
    var lastTextField = sig.fields()[lastTextFieldIndex];

    return (
      <div className="design-view-action-participant-details-information">
        {/* if */ (sig.fstnameField() != undefined || sig.sndnameField() != undefined) &&
          <ParticipantNameField
            last={lastTextField.isFstName() || lastTextField.isSndName()}
            model={sig}
          />
        }
        {/* if */ sig.emailField() != undefined &&
          <ParticipantField last={lastTextField.isEmail()} model={sig.emailField()}/>
        }
        {
          _.map(sig.fields(), function (f, i) {
            if (f.isBlank()) {
              return (<ParticipantSelectField model={f} key={"select-field" + f.cid}/>);
            } else if (f.isMobile()) {
              return (
                <ParticipantMobileField
                  last={i == lastTextFieldIndex}
                  model={f}
                  key={"field-" + f.cid}
                />
              );
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
                Track.track("Open CSV Popup");
                new CsvSignatoryDesignPopup({
                  document: self.props.document,
                  setParticipantDetail: self.props.setParticipantDetail
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
