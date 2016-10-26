var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DomainViewModel = require("../../admin/brandeddomain/domainviewmodel");
var Theme = require("../theme");
var Button = require("../../common/button");
var UploadImageButton = require("../../common/uploadimagebutton");
var InfoTextInput = require("../../common/infotextinput");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");


module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      return (
        <div className="email-preview" style={{"backgroundColor": model.brandColor()}}>
          <div className="logo-wrapper">
            <img src={ model.logo() || (window.cdnbaseurl + "/img/logo_email.png")} />
          </div>

          <div className="content-container" style={{"fontFamily": model.font()}}>
            <div className="content">
              <div className="document-preview">
                <img src={window.cdnbaseurl + "/img/document.png"} />
              </div>
              <div className="invitation">
                <p>
                  <HtmlTextWithSubstitution
                    secureText={localization.companyBranding.brandingPreview.emailContent}
                  />
                </p>
                <p>
                  <strong>{localization.companyBranding.brandingPreview.emailInstructions}</strong>
                </p>

                <Button
                  size="small"
                  type="action"
                  text={localization.companyBranding.brandingPreview.emailButtonLabel}
                  style={{'backgroundColor': model.actionColor(), 'color': model.actionTextColor()}}
                />
              </div>
            </div>

            <div className="footer">
              <p>{localization.companyBranding.brandingPreview.emailFooter}</p>
            </div>

          </div>

        </div>
      );
    }
  });
