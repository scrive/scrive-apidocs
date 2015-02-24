/** @jsx React.DOM */

define(["React","common/backbone_mixin","admin/brandeddomain/domainviewmodel","themes/theme"  ,"legacy_code","common/button","common/uploadimagebutton","common/select","common/infotextinput"], function(React, BackboneMixin, DomainViewModel,Theme,_Legacy, Button, UploadImageButton,NewSelect,InfoTextInput) {

return React.createClass({
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
        arrowFrontUrl : this.generateBackgroundImageURL("sign-arrow-action-right-front.png", model.actionColor()),
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
      return "url(/branded_image?file=" + imagePath + "&color=" + encodeURIComponent(color) + ")";
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      console.log("Rendering with url " + this.state.iconLegendMandatoryUrl);
      return (
              <div className="sample-sign-view">
                {/*if*/ (self.props.showHeader) &&

                  <div style={{"backgroundColor": model.brandColor()}} className="sample-sign-view-header">
                      <div className="logo"><img src={model.logo()} />
                      </div>
                      <div style={{"color": model.brandTextColor()}} className="header-text">{localization.sampleSignView.signViewHeader}</div>
                      <div style={{"clear": "both"}}></div>
                  </div>
                }
                <div style={{"font-family": model.font()}} className="content">
                  <div className="innercontent">
                     <div className="contentheader">
                        <div className="instructions">
                          <span>Follow the <span style={{"color": model.actionColor()}} className="arrowtext">ARROW</span></span>
                        </div>
                        {/*if*/ (self.props.showPDFDownload) &&
                          <span><a className="download" style={{color: "#333333"}}>Demo document</a></span>
                        }
                        <div className="arrow-legend">
                          <p>
                            <span className="mandatory arrow-icon" style={
                              {"background": this.state.iconLegendMandatoryUrl}
                            }/>
                            <span className="copy">{localization.sampleSignView.mandatoryAction}</span>
                          </p>
                          <p className="optionalcontainer">
                            <span className="optional arrow-icon" style={
                              {"background": this.state.iconLegendOptionalUrl}
                            }/>
                            <span className="copy">{localization.sampleSignView.optionalAction}</span>
                          </p>
                        </div>
                      </div>
                      <div className="document">
                              <div className="downarrow" style={{"background": this.state.arrowDownUrl}}/>
                              <div className="field mandatoryfield">
                                  <div className="placedfield mandatoryplacedfield" style={{"border-color": model.actionColor()}}>
                                      <div className="placedfieldvalue">{localization.sampleSignView.email}</div>
                                  </div>
                                  <div className="front" style={{"background": this.state.arrowFrontUrl}}/>
                                  <div className="label" style={{"background": this.state.arrowLabelUrl}}>{localization.sampleSignView.textField}</div>
                                  <div className="back"  style={{"background": this.state.arrowBackUrl}}/>
                              </div>
                              <div className="field optionalfield">
                                  <div className="placedfield optionalplacedfield" style={{"border-color": model.actionSecondaryColor()}}>
                                      <div className="placedfieldvalue">{localization.sampleSignView.phone}</div>
                                  </div>
                              </div><img className="exampledocument" src="/img/document_example.png" />
                              <div className="section attachments">
                                  <div className="attachments-container">
                                      <h2>{localization.sampleSignView.attachmentsTitle}</h2>
                                      <div className="file-container"><img src="/img/attachment-icon.png" className="icon" /><span className="name">{localization.sampleSignView.attachmentFilename}</span>
                                          <Button
                                      size="small"
                                      type="optional"
                                      text={localization.sampleSignView.reviewPDF}
                                      style={{'backgroundColor': model.actionSecondaryColor(), 'color': model.actionSecondaryTextColor()}}

                                    />
                                      </div>
                                  </div>
                              </div>
                              {/*if*/ (self.props.showRejectOption) &&
                                <div className="section buttons">
                                    <div className="buttoncontainer reject">
                                      <Button
                                        size="small"
                                        type="cancel"
                                        text={localization.sampleSignView.rejectButton}
                                        style={{'backgroundColor': model.negativeColor(), 'color': model.negativeTextColor()}}
                                      />
                                    </div>
                                    <div className="buttoncontainer sign">
                                      <Button
                                        size="small"
                                        type="action"
                                        text={localization.sampleSignView.signButton}
                                        style={{'backgroundColor': model.actionColor(), 'color': model.actionTextColor()}}
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

                          </div>
                      </div>
                  </div>
                  {/*if*/ (self.props.showFooter) &&
                    <div className="footer">
                      <div className="text">Powered by Scrive</div>
                    </div>
                  }
              </div>
      );
    }
  });
});
