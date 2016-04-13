var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var ViewSize = require("../viewsize");
var ReloadManager = require("../../../js/reloadmanager.js").ReloadManager;
var classNames = require("classnames");

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
      status: React.PropTypes.number.isRequired
    },

    componentDidMount: function () {
      ReloadManager.pushBlock(function () {
        return localization.signingInProgressDontCloseWindow;
      });
    },

    componentDidUpdate: function () {
      if (this.props.status > 1) {
        ReloadManager.stopBlocking();
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
          <h1>{localization.docsignview.signedNotClosed}</h1>
          <p>
            <HtmlTextWithSubstitution
              secureText={localization.signinginprogressmodal.youHaveSignedPleaseWait}
              subs={{".put-doctitle": this.props.docTitle}}
            />
          </p>
          <div className="status-box">
            <img className="img-doc" src={this.props.imgUrl} />
            <div className="status">
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
            <div className="clearfix" />
          </div>
        </div>
      );
    }
  });
