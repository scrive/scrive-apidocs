var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DomainViewModel = require("../../admin/brandeddomain/domainviewmodel");
var Theme = require("../theme");
var Button = require("../../common/button");
var UploadImageButton = require("../../common/uploadimagebutton");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");


module.exports = React.createClass({
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
                    <a className="hoverable logo"><img src={model.logo() || window.cdnbaseurl + "/img/logo_email.png"} /></a>
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
              <img src={window.cdnbaseurl + "/img/user_details_preview.png"} />
              <Button style={{backgroundColor: model.actionColor(),color: model.actionTextColor()}}size="tiny" type="action" text="Save" />
            </div>
            <div className="service-preview-footer">
                <div>Powered by Scrive</div>
            </div>
        </div>
      );
    }
  });
