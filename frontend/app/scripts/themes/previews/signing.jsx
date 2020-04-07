var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DomainViewModel = require("../../admin/brandeddomain/domainviewmodel");
var Theme = require("../theme");
var Button = require("../../common/button");
var UploadImageButton = require("../../common/uploadimagebutton");
var InfoTextInput = require("../../common/infotextinput");

var DownloadPdfIcon = require("../../icons/download_pdf_icon.svg");
var ArrowDown = require("../../icons/arrow-down.svg");
var ArrowRight = require("../../icons/arrow-right.svg");

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    getDefaultProps: function() {
      return {
        showHeader: true,
        showRejectOption: true,
        allowRejectReason: true,
        showPDFDownload: true,
        showFooter: true,
        showArrow: true
      };
    },
    componentDidUpdate: function() {
      this.updateCustomSVGColors();
    },
    componentDidMount: function() {
      this.updateCustomSVGColors();
    },
    updateCustomSVGColors: function() {
      var model = this.props.model;
      if(model.ready() && this.isMounted()) {
        const $node = $(this.getDOMNode());
        $node.find(".front .actioncolor").css("fill", model.actionColor());
        $node.find(".front .textcolor").css("fill", model.actionTextColor());
        $node.find(".downarrow .actioncolor").css("fill", model.actionColor());
      }
    },
    font: function () {
      // temporarily disable customizing fonts in signview
      return "\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif;"
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
              <div className="sample-sign-view">
                {/*if*/ (self.props.showHeader) &&

                  <div style={{"backgroundColor": model.brandColor()}} className="sample-sign-view-header">
                      <div className="logo"><img src={model.logo()} /></div>
                      <div style={{"color": model.brandTextColor()}} className="header-text">
                        {localization.sampleSignView.signViewHeader}
                      </div>
                      <div style={{"clear": "both"}}></div>
                  </div>
                }
                <div style={{"fontFamily": this.font()}} className="content">
                  <div className="innercontent">
                     <div className="sample-controls">
                       {/* if */ self.props.showPDFDownload &&
                         <div className="download-container">
                           <DownloadPdfIcon className="download-icon" />
                         </div>
                       }
                       <div className="document-title">
                         Signing document: Demo contract
                       </div>
                     </div>
                     {/*if*/ (self.props.showArrow) &&
                       <div className="section contentheader">
                         <div className="instructions">
                            <span>Follow the <span style={{"color": model.actionColor()}} className="arrowtext">ARROW</span></span>
                         </div>
                       </div>
                     }
                      <div className="document">
                              {/*if*/ (self.props.showArrow) && <ArrowDown className="downarrow"/> }
                              <div className="field mandatoryfield">
                                  <div className="placedfield mandatoryplacedfield" style={{"borderColor": model.actionColor()}}>
                                      <div className="placedfieldvalue">{localization.sampleSignView.email}</div>
                                  </div>
                                  <ArrowRight className="front"/>
                              </div>
                              <div className="field optionalfield">
                                  <div className="placedfield optionalplacedfield" style={{"borderColor": model.actionSecondaryColor()}}>
                                      <div className="placedfieldvalue">{localization.sampleSignView.phone}</div>
                                  </div>
                              </div>
                              <img className="exampledocument" src={window.cdnbaseurl + "/img/document_example.png"} />

                          </div>
                              <div className="section signsection">
                                  <Button
                                    size="small"
                                    type="action"
                                    text={localization.sampleSignView.signButton}
                                    style={{'backgroundColor': model.actionColor(), 'color': model.actionTextColor()}}
                                  />
                                  {/*if*/ (self.props.showRejectOption) &&
                                    <Button
                                      className="button reject"
                                      size="small"
                                      text={
                                        self.props.allowRejectReason ?
                                          localization.sampleSignView.rejectButton :
                                          localization.sampleSignView.rejectWithoutReasonButton
                                      }
                                    />
                                  }
                              </div>
                              {/*if*/ (self.props.showFooter) &&
                                <div className="section footer">
                                  <img className="logo" src="/img/poweredby.svg" />
                                </div>
                              }
                      </div>
                  </div>
              </div>
      );
    }
  });
