/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.object,
      forceShowing: React.PropTypes.bool // OVERRIDES Sign View Branding setting "showfooter"
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.signviewbranding];
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      
      // Show the footer if the svb is loaded and either we force showing of the footer, or
      // we're not on a small screen device and svb showfooter is set)
      var showFooter = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showfooter();

      if (signviewbranding.ready() && this.props.forceShowing) showFooter = true;

      $('.signview').toggleClass("nofooter",!showFooter); // We need to toogle this class here

      if (!showFooter || !signviewbranding.ready())
        return (<div/>);
      else
        return (
          <div className={"pagefooter " + (BrowserInfo.isSmallScreen() ? "small-screen" : "")}>
            <div className="content">
                <div className="poweredbyscrive">
                  <div className='text' > Powered by </div>
                  <span className='logo' />
                </div>
              <div className="clearfix"/>
            </div>
          </div>
      );
    }
  });

});
