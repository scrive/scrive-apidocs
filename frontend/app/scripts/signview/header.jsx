/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin', 'tinycolor'], function(React, Backbone, BackboneMixin, tinycolor) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.object,
      fullWidth : React.PropTypes.bool,
      link : React.PropTypes.object
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.signviewbranding];
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      var hasLink = this.props.link != undefined;
      var showHeader = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showheader();

      $('.signview').toggleClass("noheader",!showHeader); // We need to toogle this class here
      if (!showHeader || !signviewbranding.ready())
        return (<div/>);
      else {
        console.log("Rendering signview header " + new Date().getTime())
        return (
          <div className={"pageheader " + (this.props.fullWidth ? "full-width-header" : "")} >
            <div className="content">
              <div className="logowrapper">
                <img className="logo"
                     src={(signviewbranding.signviewlogo() != undefined && signviewbranding.signviewlogo() != "") ? signviewbranding.signviewlogo() : '/img/logo.png'}>
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
