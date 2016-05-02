var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");

  module.exports = React.createClass({
    mixins: [React.addons.LinkedStateMixin],

    propTypes: {
      onBack: React.PropTypes.func.isRequired,
      onReject: React.PropTypes.func.isRequired,
      allowRejectionReason: React.PropTypes.bool.isRequired
    },

    getInitialState: function () {
      return {text: ""};
    },

    handleReject: function () {
      this.props.onReject(this.state.text);
    },

    render: function () {
      return (
        <div>
          <div className="row">
            <div className="col-sm-12">
              <h1>{localization.process.signatorycancelmodaltitle}</h1>
            </div>
          </div>
          <div className="row">
            <div className="col-sm-6 left">
              <p>{
                this.props.allowRejectionReason ?
                  localization.process.signatorycancelmodaltext :
                  localization.process.signatorycancelmodaltextwithoutreason
                }
              </p>
            </div>
            { /* IF */ this.props.allowRejectionReason &&
              <div className="col-sm-6 right">
                <div className="reject-textarea">
                  <p className="label"><label htmlFor="text">{localization.signviewMessage}</label></p>
                  <textarea
                    id="text"
                    valueLink={this.linkState("text")}
                    className="signview-textarea"
                    placeholder={localization.process.signatorycancelmodalplaceholder}
                  />
                </div>
              </div>
            }
          </div>
          <div className="row">
            <div className="col-sm-12 right">
              <div className="button-group">
                <Button
                  className="button-reject"
                  text={localization.reject.send}
                  onClick={this.handleReject}
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
