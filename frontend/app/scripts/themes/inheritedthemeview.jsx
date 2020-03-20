var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var EmailPreview = require("./previews/email");
var SigningPreview = require("./previews/signing");
var LoginPreview = require("./previews/login");
var ServicePreview = require("./previews/service");


module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    previewTitle : function(preview) {
      if (preview == EmailPreview) {
        return localization.branding.themes.emailPreview;
      } else if (preview == SigningPreview) {
        return localization.branding.themes.signviewPreview;
      } else if (preview == ServicePreview) {
        return localization.branding.themes.servicePreview;
      } else if (preview == LoginPreview) {
        return localization.branding.themes.loginPreview;
      }
      return "";
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className="theme-edit">
          <div className="previews">
            <div key={"preview"}>
               <div className="preview-title">{self.previewTitle(this.props.preview)}</div>
                    {React.createElement(this.props.preview,{model: model})}
            </div>

          </div>

        </div>
      );
    }
  });
