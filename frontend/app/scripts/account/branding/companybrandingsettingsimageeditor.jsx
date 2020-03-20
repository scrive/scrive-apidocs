var React = require("react");
var UploadImageButton = require("../../common/uploadimagebutton");


module.exports = React.createClass({
  propTypes: {
    title: React.PropTypes.string,
    description: React.PropTypes.string,
    uploadText: React.PropTypes.string,
    alternativeImage: React.PropTypes.string,
    getValue: React.PropTypes.func,
    setValue: React.PropTypes.func,
    readonly: React.PropTypes.bool
  },
  render: function() {
    var self = this;
    return (
      <div className="companybranding-property-editor companybranding-image-property">
        <div className="companybranding-image-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="companybranding-text-property-edit">
          <img src={this.props.getValue() || this.props.alternativeImage} className="favicon-image"/>
          { !self.props.readonly &&
              <UploadImageButton
                text={self.props.uploadText}
                width={160}
                size="tiny"
                unsupportedFileMessage={localization.imageFileFormatNotSpportedIco}
                extensions={["png", "ico"]}
                onUploadComplete={function(image) {
                  self.props.setValue(image);
                }}
              />
          }
        </div>
      </div>
    );
  }
});
