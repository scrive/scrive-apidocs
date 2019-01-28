var React = require("react");
var Backbone = require("backbone");



  module.exports = React.createClass({
    propTypes: {
      linkText : React.PropTypes.string,
      linkOnClick : React.PropTypes.func,
      documentid : React.PropTypes.string
    },
    logoLink : function() {
      if (!this.props.documentid) {
        return window.cdnbaseurl + "/signview_logo_without_document/" + window.brandingdomainid + "/" + window.brandinguserid + "/" + window.brandinghash;
      } else {
        return window.cdnbaseurl + "/signview_logo/" + window.brandingdomainid + "/" + this.props.documentid + "/" + window.brandinghash;
      }
    },
    render: function() {
      return (
        <div className="pageheader full-width-header">
          <div className="content">
            <div className="logowrapper">
              <img className="logo"
                    src={this.logoLink()}>
              </img>
            </div>
            {/* if */ this.props.linkText &&
              <div className="sender">
                <div className="inner clickable" onClick={this.props.linkOnClick}>
                    <a className="link">
                      {this.props.linkText}
                    </a>
                </div>
              </div>
            }
            <div className="clearfix"/>
          </div>
        </div>
      );
    }
  });
