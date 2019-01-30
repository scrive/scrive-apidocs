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

    handleForward: function () {
      this.props.onForward(this.state.messageText, this.newFieldsWithValuesForUpdate());
    },

    newFieldsWithValuesForUpdate: function () {
      var self = this;
      var sig = this.props.model.document().currentSignatory();
      var textFiels = sig.fields().filter((f) => f.isText());
      var res = [];
      _.each(textFiels, function (f) {
        if (self.getNewValue(f) != null && f.value() != self.getNewValue(f)) {
          res.push({
            field: f,
            newValue: self.getNewValue(f)
          });
        }
      });
      return res;
    },

    fieldKey: function (f) {
        return (f.type() + "_" + (f.order() || "") + "_" +  (f.name() || ""));
    },
    getNewValue (f) {
        return this.state.newValues[this.fieldKey(f)];
    },

    setNewValue: function (f, value) {
      var clone = _.clone(this.state.newValues);
      clone[this.fieldKey(f)] = value;
      this.setState({newValues: clone});
    },

    render: function () {
      var self = this;
      var sig = this.props.model.document().currentSignatory();
      var textFiels = sig.fields().filter((f) => f.isText());

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
                  {_.map(textFiels, function (tf) {
                    return (
                      <div>
                        <label style={{"width": "100px"}}>{tf.nicename()}</label>
                        <InfoTextInput
                          style={{"width": "250px"}}
                          infotext={tf.nicename()}
                          value={self.getNewValue(tf) !== undefined ? self.getNewValue(tf) : tf.value()}
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
                  className="button-forward"
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
