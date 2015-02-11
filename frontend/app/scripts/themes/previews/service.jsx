/** @jsx React.DOM */

define(["React","common/backbone_mixin","admin/brandeddomain/domainviewmodel","themes/theme"  ,"legacy_code","common/button","common/uploadimagebutton","common/select","common/infotextinput"], function(React, BackboneMixin, DomainViewModel,Theme,_Legacy, Button, UploadImageButton,NewSelect,InfoTextInput) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      var buttonStyles = {
        'backgroundColor': model.brandColor(),
        'borderColor': model.brandTextColor()
      };

      return (
        <div className="service-preview">
            <div className="service-preview-header" style={{"backgroundColor": model.brandColor()}}>
                <div className="service-preview-logo">
                    <a className="hoverable logo"><img src={model.logo() || "/img/logo_email.png"} /></a>
                </div>
                <div className="service-preview-innerheader">
                    <Button className="service-preview-header-button" size="tiny" type="main" text="Start new process" textcolor={model.brandTextColor()} style={buttonStyles}/>
                    <Button className="service-preview-header-button" size="tiny" type="main" text="Start from template" textcolor={model.brandTextColor()} style={buttonStyles}/>
                    <div className="service-preview-header-text" style={{"color": model.brandTextColor()}}><a className="hoverable">E-archive</a> </div>
                    <div className="service-preview-header-text" style={{"color": model.brandTextColor()}}><a className="hoverable">Account</a> </div>
                    <div className="service-preview-header-text" style={{"color": model.brandTextColor()}}><a className="hoverable">Log out</a></div>
                </div>
            </div>
            <div className="service-preview-content">
              <img src="/img/user_details_preview.png" />
              <Button style={{backgroundColor: model.actionColor(),color: model.actionTextColor()}}size="tiny" type="action" text="Save" />
            </div>
            <div className="service-preview-footer">
                <div>Powered by Scrive</div>
            </div>
        </div>
      );
    }
  });
});
