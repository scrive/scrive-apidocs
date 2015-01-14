/** @jsx React.DOM */

define(["React","legacy_code","common/uploadimagebutton"], function(React, _Legacy, UploadImageButton) {

return React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-image-property">
        <div className="domain-image-property-title">
          <strong>{self.props.title}:</strong>
        </div>
        <div className="domain-text-property-edit">
          <img src={this.props.getValue()} className="favicon-image"/>
          <UploadImageButton
                text="Upload image"
                width={160}
                size="tiny"
                onUpload={function(image) {
                  self.props.setValue(image);
                }}
          />
        </div>
      </div>
    );
  }
});
});
