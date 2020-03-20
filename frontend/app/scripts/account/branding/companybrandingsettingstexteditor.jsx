var React = require("react");
var InfoTextInput = require("../../common/infotextinput");


module.exports = React.createClass({
  propTypes: {
    title: React.PropTypes.string,
    description: React.PropTypes.string,
    maxLength: React.PropTypes.number,
    getValue: React.PropTypes.func,
    setValue: React.PropTypes.func,
    readonly: React.PropTypes.bool
  },
  render: function() {
    var self = this;
    return (
      <div className="companybranding-property-editor domain-text-property">
        <div className="companybranding-text-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="companybranding-text-property-edit">
          <InfoTextInput
            value={self.props.getValue()}
            infotext=""
            onChange={function(v) {self.props.setValue(v)}}
            maxLength={self.props.maxLength}
            readonly={self.props.readonly}
          />
        </div>
      </div>
    );
  }
});
