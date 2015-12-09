define(["Underscore", "Backbone", "React", "common/button"],
  function (_, Backbone, React, Button) {

  return React.createClass({
    mixins: [React.addons.LinkedStateMixin],

    propTypes: {
      onBack: React.PropTypes.func.isRequired,
      onReject: React.PropTypes.func.isRequired
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
            <div className="col-xs-12">
              <h1>{localization.process.signatorycancelmodaltitle}</h1>
            </div>
          </div>
          <div className="row">
            <div className="col-xs-5 left">
              <p>{localization.process.signatorycancelmodaltext}</p>
            </div>
            <div className="col-xs-7 right">
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
          </div>
          <div className="row">
            <div className="col-xs-12 right">
              <div className="button-group">
                <Button
                  className="transparent-button"
                  text={localization.toStart.backFromSigningPage}
                  onClick={this.props.onBack}
                />
                <Button
                  className="button-reject"
                  text={localization.reject.send}
                  onClick={this.handleReject}
                />
              </div>
            </div>
          </div>
        </div>
      );
    }
  });
});
