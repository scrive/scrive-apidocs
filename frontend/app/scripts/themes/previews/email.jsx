/** @jsx React.DOM */

define(["React", "common/backbone_mixin", "admin/brandeddomain/domainviewmodel", "themes/theme",
        "legacy_code", "common/button", "common/uploadimagebutton", "common/select",
        "common/infotextinput", "common/htmltextwithsubstitution"],
       function(React, BackboneMixin, DomainViewModel,Theme, _Legacy, Button,
                UploadImageButton, Select, InfoTextInput, HtmlTextWithSubstitution) {

return React.createClass({
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
            <img src={ model.logo() || window.cdnbaseurl + "/img/logo_email.png"} />
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
                  {localization.companyBranding.brandingPreview.emailInstructions}
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
});
