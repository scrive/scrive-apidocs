/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin', 'tinycolor'], function(React, Backbone, BackboneMixin, tinycolor) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.object,
      fullWidth : React.PropTypes.bool,
      link : React.PropTypes.object,
      forceShowing: React.PropTypes.bool // Overrides sign view branding setting "showheader"
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.signviewbranding];
    },
    logoLink : function() {
      if (this.props.documentid && this.props.signatorylinkid) {
        return "/signview_logo/" + this.props.documentid + "/" + this.props.signatorylinkid + "/" + window.brandinghash;
      } else {
        return "/signview_logo_without_document/" + window.brandinghash;
      }
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      var hasLink = this.props.link != undefined;

      // In usual cases we don't show the header (with the logo etc) on small screen devices, but some cases (to-sign and to-start view) override this behaviour.
      var showHeader = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showheader();

      if (signviewbranding.ready() && this.props.forceShowing) showHeader = true;

      $('.signview').toggleClass("noheader",!showHeader); // We need to toogle this class here
      if (!showHeader || !signviewbranding.ready())
        return (<div/>);
      else {
        return (
          <div className={"pageheader " + (this.props.fullWidth ? "full-width-header" : "")} >
            <div className="content">
              <div className="logowrapper">
                <img className="logo"
                     src={this.logoLink()}>
                </img>
              </div>
              {/*if*/ hasLink &&
                <div className="sender">
                  <div className="inner clickable" onClick={this.props.link.onClick}>
                      <a className='link'>
                        {this.props.link.text}
                      </a>
                  </div>
                </div>
              }
              {/*else*/ !hasLink &&
                <div className="sender">
                  <div className="inner">
                    <div className='name'>
                      {signviewbranding.fullname()}
                    </div>
                    <div className='phone'>
                      {signviewbranding.phone()}
                    </div>
                  </div>
                </div>
              }
              <div className="clearfix"/>
            </div>
          </div>
      );
      }
    }
  });

});
