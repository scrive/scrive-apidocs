/** @jsx React.DOM */

define(["React","legacy_code","common/uploadimagebutton"], function(React, _Legacy,UploadImageButton) {


return React.createClass({
  propTypes: {
    getLogo: React.PropTypes.func,
    setLogo: React.PropTypes.func,
    getBackgroundColor: React.PropTypes.func
  },
  render: function() {
    var self = this;
    var model = this.props.model;
    return (
      <div className="theme-logo-editor">
        <div className="title theme-logo-editor-title">
          {localization.branding.themes.logo}
        </div>
        <div className="theme-logo-editor-edit">
          <div
            className="logo-sample"
            style={{backgroundColor: this.props.getBackgroundColor()}}
            >
            <img src={this.props.getLogo()}/>
          </div>
          <div className="theme-edit-logo-upload">
             <UploadImageButton
                text={localization.branding.themes.uploadNewLogo}
                width={140}
                size="tiny"
                onUploadComplete={function(logo) {
                  self.props.setLogo(logo);
                }}
              />
          </div>
        </div>
      </div>
    );
  }
});

});
