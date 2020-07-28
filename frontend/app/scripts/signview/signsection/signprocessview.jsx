var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var ViewSize = require("../viewsize");
var ReloadManager = require("../../../js/reloadmanager.js").ReloadManager;
var classNames = require("classnames");
import {toLessInteropLoader} from "../../common/less_utils.jsx";
import signVars_ from "!@hon2a/less-vars-loader!../../../less/signview/sign.less";
const signVars = toLessInteropLoader(signVars_);

  var Status = React.createClass({
    render: function () {
      if (this.props.done) {
        return <div className="check-done" />;
      }

      if (this.props.active) {
        return <img src={window.cdnbaseurl + "/img/wait30trans.gif"} />;
      }

      return <span />;
    }
  });

  module.exports = React.createClass({
    propTypes: {
      imgUrl: React.PropTypes.string.isRequired,
      docTitle: React.PropTypes.string.isRequired,
      pageHeight: React.PropTypes.number.isRequired,
      pageWidth: React.PropTypes.number.isRequired,
      status: React.PropTypes.number.isRequired
    },

    componentDidMount: function () {
      ReloadManager.pushBlock(function () {
        return localization.signingInProgressDontCloseWindow;
      });
      this.fixHeight();
    },

    componentDidUpdate: function () {
      if (this.props.status > 1) {
        ReloadManager.stopBlocking();
      }
      this.fixHeight();
    },

    fixHeight: function () {
      // we need to set explicit image height
      // so other calculations (like scrolling) can work properly even if
      // the image is not yet loaded

      var $this = $(this.getDOMNode());
      var modalWidth = $this.width();

      // ratio of image width to this whole div
      var imgWidthScale = signVars.signviewSignPreviewImageWidth.replace("%", "") / 100;

      // how much was the image scaled down from its original size based on its width
      var scaleRatio = (modalWidth / this.props.pageWidth) * imgWidthScale;

      var img = this.refs.imgdoc;
      if (img !== undefined) {
        $(img.getDOMNode()).css("height", this.props.pageHeight * scaleRatio);
      }
    },

    render: function () {
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <div>
            <img className="img-doc" src={this.props.imgUrl} ref="imgdoc"/>
          </div>
          <div className="status-box">
            <div className="status">
              <h1>{localization.docsignview.signedNotClosed}: {this.props.docTitle}</h1>
              <div className="vertical">
                <div className="middle">
                  <table className="list">
                    <tbody>
                      <tr>
                        <td><Status active={this.props.status === 0} done={this.props.status > 0} /></td>
                        <td>{localization.signinginprogressmodal.action1}</td>
                      </tr>
                      <tr>
                        <td><Status active={this.props.status === 1} done={this.props.status > 1} /></td>
                        <td>{localization.signinginprogressmodal.action2}</td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          </div>
          <div className="clearfix" />
        </div>
      );
    }
  });
