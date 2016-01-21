/** @jsx React.DOM */


define(['React', 'Backbone'], function(React, Backbone) {

  return React.createClass({
    propTypes: {
      linkText : React.PropTypes.string,
      linkOnClick : React.PropTypes.func
    },
    logoLink : function() {
      // FIXME signview_logo_without_document needs userid, however I can find
      // proper usage of this React component anywhere that would make sense...
      // Otherwise I could pass userid into this component to use for the URL
      return window.cdnbaseurl + "/signview_logo_without_document/" + window.brandingdomainid + "/" + window.brandinghash;
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

});
