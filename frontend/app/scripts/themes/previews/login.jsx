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
   getInitialState: function() {
      console.log("Init new preview");
      return this.updatedState();
    },
    updatedState : function() {
      var model = this.props.model;
      return {
        iconBrandTextColor : model.brandTextColor(),
        dividerLineBackgroundUrl : this.generateBackgroundImageURL("divider-line.png", model.brandTextColor()),
        iconSelectArrowUrl : this.generateBackgroundImageURL("select-arrow-for-branding.png", model.brandTextColor()),
        updateTime : new Date().getTime()
      };
    },
    componentDidUpdate: function() {
      var self = this;
      self._updateCounter = self._updateCounter || 0;
      self._updateCounter++;
      var currentUpdateId = self._updateCounter;
      if (this.state.iconBrandTextColor != self.props.model.brandTextColor()) {
        setTimeout(function() {
          // We will update state only if this is latest update or last update happend more then 200ms ago
          if (self._updateCounter == currentUpdateId || (Math.abs(new Date().getTime() - self.state.updateTime) > 200)) {
            self.setState(self.updatedState())}
          }
        ,(Math.abs(new Date().getTime() - self.state.updateTime) > 1000) ? 0 : 200); // We shorten the timeout, if color has not been changed for at least 1 sec.
      }
    },
    generateBackgroundImageURL: function(imagePath, color) {
      return "url(/colored_image?file=" + imagePath + "&color=" + encodeURIComponent(color) + ")";
    },
    render: function() {
      var self = this;
      var model = this.props.model;

      return (
        <div className="login-preview" style={{"backgroundColor": model.brandColor()}}>
          <div className="logo-wrapper">
            <img src={ model.logo() || window.cdnbaseurl + "/img/logo_email.png"}/>
            <div className="divider-line" style={{"backgroundImage": this.state.dividerLineBackgroundUrl}} />

            <p className="small" style={{"color": model.brandTextColor()}}>E-signing powered by Scrive</p>
          </div>

          <div className="content-container" style={{"fontFamily": model.font()}}>
            <div className="content">
              <InfoTextInput infotext="Email address" style={{"width":"200px"}} readonly={true} disabled={true} />
              <InfoTextInput infotext="Password" style={{"width": "200px"}} readonly={true} disabled={true} buttonTitle="Forgot?" onButtonClick={function() {}} />
              <Button type="main" text="Log in" style={{"backgroundColor": model.brandColor(), "borderColor": model.brandTextColor(), "color": model.brandTextColor()}}/>
              <p style={{"color": model.brandTextColor()}}>Don't have an account? <a style={{"color": model.brandTextColor()}}>Sign up for free</a></p>

              <div className="select-container">
                <div className="select " style={{"width":"150px", "backgroundColor": model.brandColor(), "border": ("1px solid " +model.brandTextColor()), "color": model.brandTextColor()}}>
                  <div className="select-button">
                    <div className="select-button-left"></div>
                    <div className="select-button-label" style={{"width":"150px", "backgroundColor": model.brandColor(), "color": model.brandTextColor()}}>English</div>
                    <div className="select-button-right" style={{"backgroundImage": this.state.iconSelectArrowUrl}}></div>
                  </div>
                </div>
              </div>
            </div>
          </div>

        </div>
      );
    }
  });
