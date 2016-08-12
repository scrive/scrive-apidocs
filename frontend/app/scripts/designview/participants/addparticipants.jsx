var React = require("react");
var Button = require("../../common/button");
var Signatory = require("../../../js/signatories.js").Signatory;
var CsvSignatoryDesignPopup = require("../../../js/designview/csvsignatorydesign.js").CsvSignatoryDesignPopup;
var _ = require("underscore");

module.exports = React.createClass({
  onDone: function () {
    mixpanel.track("Close participant");
    this.props.setParticipantDetail(undefined);
  },
  addSingleParticipant: function () {
    var sig = new Signatory({
      document: this.props.document,
      signs: true
    });
    this.props.document.addExistingSignatory(sig);
    this.props.setParticipantDetail(sig);
    this.props.onAddSingle();
  },
  addMultisendParticipant: function () {
    mixpanel.track("Click add CSV");
    new CsvSignatoryDesignPopup({
      document: this.props.document,
      setParticipantDetail: this.props.setParticipantDetail
    });
  },
  render: function () {
    var self = this;

    return (
      <div className="design-view-action-participant-new-box-buttons">
        {/* if */ this.props.currentParticipantDetail != undefined &&
          <div className="design-view-action-participant-done">
            <Button
               ref="close-button"
               type="action"
               text={localization.designview.addParties.close}
               onClick={function () { self.onDone(); }}
            />
          </div>
        }
        {/* else */ this.props.currentParticipantDetail == undefined &&
          <div>
            {/* if */ !_.any(this.props.document.signatories(), function (x) { return x.isCsv(); }) &&
              <div className="design-view-action-participant-new-multi">
                <Button
                  ref="add-multi-button"
                  text={localization.designview.addMultisend}
                  onClick={function () { self.addMultisendParticipant(); }}
                />
              </div>
            }

            <div className="design-view-action-participant-new-single">
              <Button
                ref="add-single-button"
                type="action"
                text={localization.designview.addParty}
                onClick={function () { self.addSingleParticipant(); }}
              />
            </div>

          </div>
        }
      </div>
    );
  }
});
