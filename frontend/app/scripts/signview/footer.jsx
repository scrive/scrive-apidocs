/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.object
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.signviewbranding];
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      var showFooter = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showfooter();

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
