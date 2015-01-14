/** @jsx React.DOM */

define(["React","legacy_code","common/uploadimagebutton"], function(React,_Legacy, UploadImageButton) {

return React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="companybranding-property-editor companybranding-image-property">
        <div className="companybranding-image-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="companybranding-text-property-edit">
          <img src={this.props.getValue() || this.props.alternativeImage} className="favicon-image"/>
          <UploadImageButton
                text={self.props.uploadText}
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
