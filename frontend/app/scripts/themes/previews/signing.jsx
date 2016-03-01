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
    getDefaultProps: function() {
      return {
        showHeader: true,
        showRejectOption: true,
        showPDFDownload: true,
        showFooter: true
      };
    },
    getInitialState: function() {
      return this.updatedState();
    },
    updatedState : function() {
      var model = this.props.model;
      return {
        iconActionColor : model.actionColor(),
        iconLegendMandatoryUrl : this.generateBackgroundImageURL("icon-legend-mandatory.png", model.actionColor()),
        iconLegendOptionalUrl  : this.generateBackgroundImageURL("icon-legend-optional.png", model.actionSecondaryColor()),
        arrowFrontUrl : this.generateBackgroundImageURL("sign-arrow-action-right.png", model.actionColor()),
        arrowLabelUrl: this.generateBackgroundImageURL("sign-arrow-action-label.png", model.actionColor()),
        arrowBackUrl: this.generateBackgroundImageURL("sign-arrow-action-right-back.png", model.actionColor()),
        arrowDownUrl:  this.generateBackgroundImageURL("sign-arrow-down.png", model.actionColor()),
        updateTime : new Date().getTime()
      };
    },
    componentDidUpdate: function() {
      var self = this;
      self._updateCounter = self._updateCounter || 0;
      self._updateCounter++;
      var currentUpdateId = self._updateCounter;
      if (this.state.iconActionColor != self.props.model.actionColor()) {
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
              <div className="sample-sign-view">
                {/*if*/ (self.props.showHeader) &&

                  <div style={{"backgroundColor": model.brandColor()}} className="sample-sign-view-header">
                      <div className="logo"><img src={model.logo()} /></div>
                      <div style={{"color": model.brandTextColor()}} className="header-text">{localization.sampleSignView.signViewHeader}</div>
                      <div style={{"clear": "both"}}></div>
                  </div>
                }
                <div style={{"fontFamily": model.font()}} className="content">
                  <div className="innercontent">
                     <div className="section contentheader">
                        <div className="instructions">
                          <span>Follow the <span style={{"color": model.actionColor()}} className="arrowtext">ARROW</span></span>
                          <div className="document-name">Demo document</div>
                          {/* if */ self.props.showPDFDownload &&
                            <Button
                              size="small"
                              text={localization.docsignview.downloadDocumentButtonText}
                            />
                          }
                        </div>
                      </div>
                      <div className="document">
                              <div className="downarrow" style={{"background": this.state.arrowDownUrl}}/>
                              <div className="field mandatoryfield">
                                  <div className="placedfield mandatoryplacedfield" style={{"borderColor": model.actionColor()}}>
                                      <div className="placedfieldvalue">{localization.sampleSignView.email}</div>
                                  </div>
                                  <div className="front" style={{"background": this.state.arrowFrontUrl}}/>
                              </div>
                              <div className="field optionalfield">
                                  <div className="placedfield optionalplacedfield" style={{"borderColor": model.actionSecondaryColor()}}>
                                      <div className="placedfieldvalue">{localization.sampleSignView.phone}</div>
                                  </div>
                              </div><img className="exampledocument" src={window.cdnbaseurl + "/img/document_example.png"} />

                          </div>
                              {/*if*/ (self.props.showRejectOption) &&
                                <div className="section buttons">
                                    <div className="buttoncontainer sign">
                                      <Button
                                        size="small"
                                        type="action"
                                        text={localization.sampleSignView.signButton}
                                        style={{'backgroundColor': model.actionColor(), 'color': model.actionTextColor()}}
                                      />
                                    </div>
                                    <div className="buttoncontainer reject">
                                      <Button
                                        size="small"
                                        text={localization.sampleSignView.rejectButton}
                                      />
                                    </div>
                                </div>
                              }
                              {/*else*/ (!self.props.showRejectOption) &&
                                <div className="section buttons">
                                      <div className="buttoncontainer sign" style={{"float": "none"}}>
                                      <Button
                                        size="small"
                                        type="action"
                                        text={localization.sampleSignView.signButton}
                                        style={{'backgroundColor': model.actionColor(), 'color': model.actionTextColor()}}
                                      />
                                    </div>
                                </div>
                              }
                              {/*if*/ (self.props.showFooter) &&
                                <div className="section footer">
                                  <img className="logo" src="/img/poweredby.png" />
                                </div>
                              }
                      </div>
                  </div>
              </div>
      );
    }
  });
