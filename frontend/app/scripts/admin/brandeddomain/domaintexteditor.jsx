/** @jsx React.DOM */

define(["React","legacy_code","common/infotextinput"], function(React,_Legacy, InfoTextInput) {

return  React.createClass({
  propTypes: {
    title: React.PropTypes.string,
    description: React.PropTypes.string,
    maxLength: React.PropTypes.number,
    getValue: React.PropTypes.func,
    setValue: React.PropTypes.func
  },
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-text-property">
        <div className="domain-text-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="domain-text-property-edit">
          <InfoTextInput
            value={self.props.getValue()}
            infotext=""
            onChange={function(v) {self.props.setValue(v)}}
            maxLength={self.props.maxLength}
          />
        </div>
      </div>
    );
  }
});

});
