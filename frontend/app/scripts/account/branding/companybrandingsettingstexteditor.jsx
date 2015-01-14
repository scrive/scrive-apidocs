/** @jsx React.DOM */

define(["React","legacy_code","common/infotextinput"], function(React,_Legacyt,InfoTextInput) {

return React.createClass({
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
          />
        </div>
      </div>
    );
  }
});

});
