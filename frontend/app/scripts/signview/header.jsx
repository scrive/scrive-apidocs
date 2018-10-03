var React = require("react");
var Backbone = require("backbone");
var BackboneMixin = require("../common/backbone_mixin");
var _ = require("underscore");
var ModelObserverMixin = require("./model_observer_mixin");

  module.exports = React.createClass({
    displayName: "Header",
    mixins: [ModelObserverMixin],
    propTypes: {
      document: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      documentid: React.PropTypes.string.isRequired,
      link : React.PropTypes.object,
      authorFullname: React.PropTypes.string,
      authorPhone: React.PropTypes.string,
      hasDownloadButton: React.PropTypes.bool
    },
    shouldComponentUpdate: function (nextProps, nextState) {
      var observedAttrs = [
        "ready", "showpdfdownload", "status", "sealedfile", "file"
      ];

      return this.hasChanges(this.props.document, observedAttrs);
    },
    logoLink: function() {
      return window.cdnbaseurl + "/signview_logo/" + window.brandingdomainid + "/" + this.props.documentid + "/" + window.brandinghash;
    },
    isReady: function () {
      return this.props.document.ready() && this.props.document.mainfile() != undefined;
    },
    render: function() {
      var doc = this.props.document;
      var hasLink = this.props.link != undefined;

      return (
        <div className={"header"} >
          <div className="main">
            <div className="row">
              <div className="col-xs-6 left">
                <div className="vertical left">
                  <div className="middle">
                    <img crossOrigin="anonymous" className="logo" src={this.logoLink()} />
                  </div>
                </div>
              </div>
              <div className="col-xs-6 content">
                <div className="vertical">
                  <div className="middle">
                    <div className="info">
                      {/* if */ hasLink &&
                        <div className="middle">
                          <a className="link" onClick={this.props.link.onClick}>
                          {this.props.link.text}
                          </a>
                        </div>
                      }
                      {/* else */ !hasLink &&
                        <div className="middle">
                            <h4>{_.unescape(this.props.authorFullname)}</h4>
                            <h4>{_.unescape(this.props.authorPhone)}</h4>
                        </div>
                      }
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div className="clearfix" />
        </div>
    );
    }
  });
