/** @jsx React.DOM */

define(["React","legacy_code","common/uploadimagebutton"], function(React, _Legacy, UploadImageButton) {

return React.createClass({
  propTypes: {
    title: React.PropTypes.string,
    description: React.PropTypes.string,
    uploadText: React.PropTypes.string,
    getValue: React.PropTypes.func,
    setValue: React.PropTypes.func
  },
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-image-property">
        <div className="domain-image-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="domain-text-property-edit">
          <img src={this.props.getValue()} className="favicon-image"/>
          <UploadImageButton
                text={self.props.uploadText} 
                width={160}
                size="tiny"
                onUploadComplete={function(image) {
                  self.props.setValue(image);
                }}
          />
        </div>
      </div>
    );
  }
});
});
