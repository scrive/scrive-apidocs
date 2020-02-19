var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var InfoTextInput = require("../../common/infotextinput");

  module.exports = React.createClass({
    mixins: [React.addons.LinkedStateMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onForward: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      return {messageText: "", newValues: {}};
    },

    allFieldsValid: function () {
      var self = this;
      var sig = this.props.model.document().currentSignatory();
      var textFields = sig.fields().filter((f) => f.isStandard());
      return _.every(textFields, (tf) => tf.isValid(self.getNewValue(tf) || ""));
    },

    handleForward: function () {
      if (this.allFieldsValid()) {
        this.props.onForward(this.state.messageText, this.newFieldsWithValuesForUpdate());
      }
    },

    newFieldsWithValuesForUpdate: function () {
      var self = this;
      var sig = this.props.model.document().currentSignatory();
      var textFields = sig.fields().filter((f) => f.isStandard());
      var res = [];
      _.each(textFields, function (f) {
        var newVal = self.getNewValue(f);
        // send empty string if not filled out, so backend does not default to old sig values
        if (f.value() != newVal) {
          res.push({
            field: f,
            newValue: newVal !== undefined ? newVal : ""
          });
        }
      });
      return res;
    },

    fieldKey: function (f) {
        return (f.type() + "_" + (f.order() || "") + "_" +  (f.name() || ""));
    },
    getNewValue (f) {
      if (f.isCustom() && f.value() !== "") {
        return f.value();
      }
      return this.state.newValues[this.fieldKey(f)];
    },

    setNewValue: function (f, value) {
      var clone = _.clone(this.state.newValues);
      clone[this.fieldKey(f)] = value;
      this.setState({newValues: clone});
    },

    obligatoryField: function (f) {
      return f.requiredForParticipation();
    },

    render: function () {
      var self = this;
      var sig = this.props.model.document().currentSignatory();
      var textFields = sig.fields().filter((f) => f.isStandard());

      return (
        <div>
          <div className="row">
            <div className="col-sm-12">
              <h1>{localization.forward.forward}</h1>
              <p>{sig.signs() ? localization.forward.forwardtootherperson
                              : localization.forward.forwardtootherpersonapproving}</p>

            </div>
          </div>
          <div className="row">
            <div className="col-sm-12">
              <div className="forward-textarea">
                  <p className="label"><label htmlFor="text">{localization.forward.otherpersondetails}</label></p>
                  {_.map(textFields, function (tf) {
                    return (
                      <div>
                        <label style={{"width": "100px"}}>{tf.nicename()}</label>
                        <InfoTextInput
                          style={{"width": "250px"}}
                          infotext={tf.nicename()}
                          className={(self.obligatoryField(tf) ? "obligatory-forward-input" : "") +
                                     (tf.isValid(self.getNewValue(tf) || "") ? " valid" : "")}
                          value={self.getNewValue(tf)}
                          disabled={tf.isCustom() && tf.value() !== ""}
                          onChange={function (v) { self.setNewValue(tf, v); }}
                        />
                      </div>
                    );
                  })}

              </div>
            </div>
          </div>
          <br/>
          <br/>
          <div className="row">
            <div className="col-sm-12">
              <div className="forward-textarea">
                <p className="label"><label htmlFor="text">{localization.forward.messageforotherperson}</label></p>
                <textarea
                  id="text"
                  valueLink={this.linkState("messageText")}
                  className="signview-textarea"
                  placeholder={localization.forward.typeyourmessagehere}
                />
              </div>
            </div>
          </div>
          <div className="row">
            <div className="col-sm-12 right">
              <div className="button-group">
                <Button
                  className={"button-forward" + (self.allFieldsValid() ? " action" : "")}
                  text={localization.forward.send}
                  onClick={this.handleForward}
                />
                <Button
                  className="transparent-button"
                  text={localization.toStart.backFromSigningPage}
                  onClick={this.props.onBack}
                />
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
