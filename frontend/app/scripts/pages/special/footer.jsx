/** @jsx React.DOM */


define(['React', 'Backbone'], function(React, Backbone) {

  return React.createClass({
    propTypes: {
      forceShowing: React.PropTypes.bool // OVERRIDES Sign View Branding setting "showfooter"
    },
    render: function() {
      return (
        <div className={"pagefooter " + (BrowserInfo.isSmallScreen() ? "small-screen" : "")}>
          <div className="content">
              <div className="poweredbyscrive">
                <div className="text" > Powered by </div>
                <span className="logo"/>
              </div>
            <div className="clearfix"/>
          </div>
        </div>
      );
    }
  });

});
