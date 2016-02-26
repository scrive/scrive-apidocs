var React = require("react");
var Backbone = require("backbone");



  module.exports = React.createClass({
    propTypes: {
      linkText : React.PropTypes.string,
      linkOnClick : React.PropTypes.func
    },
    logoLink : function() {
      return window.cdnbaseurl + "/signview_logo_without_document/" + window.brandingdomainid + "/" + window.brandinguserid + "/" + window.brandinghash;
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
            <div className="sender">
              <div className="inner clickable" onClick={this.props.linkOnClick}>
                  <a className="link">
                    {this.props.linkText}
                  </a>
              </div>
            </div>
            <div className="clearfix"/>
          </div>
        </div>
      );
    }
  });
