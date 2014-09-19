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
    componentDidMount : function() {
      // HACK!!!
      // Chrome on OSX has a bug, that displays some garbage in the header
      // (weird straight white line in the right part of the header)
      // let's force a redraw to make it disappear
      var pageheader = this.getDOMNode();
      var self = this;
      setTimeout(function() {
        var signviewbranding = self.props.signviewbranding;
        var bgColor = signviewbranding.signviewbarscolour() != undefined ?  signviewbranding.signviewbarscolour() : '#495259'; // @brand-primary from variables.less
        bgColor = tinycolor(bgColor).toRgbString();

        // figrue out a temporary colour that is similar to the original (so the flicker is not that visible), but still different
        var tmpColor = tinycolor.lighten(bgColor, 1).toRgbString();
        if (tmpColor === bgColor) {
          // bgColor was too close to white and the difference could be not big enough, darken it instead
          tmpColor = tinycolor.darken(bgColor, 1).toRgbString();
        }
        jQuery(pageheader).css('background', tmpColor);
        setTimeout(function() {
          jQuery(pageheader).css('background', bgColor);
        }, 1);
      }, 1000);
      // END OF HACK
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      var hasLink = this.props.link != undefined;
      var showHeader = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showheader();

      $('.signview').toggleClass("noheader",!showHeader); // We need to toogle this class here

      var bgImage = signviewbranding.signviewbarscolour() != undefined ?  'none' : '';
      var bgColor = signviewbranding.signviewbarscolour() != undefined ?  signviewbranding.signviewbarscolour() : '';
      var color = signviewbranding.signviewbarstextcolour() != undefined ? signviewbranding.signviewbarstextcolour() : '';
      var font = signviewbranding.signviewtextfont() != undefined ? signviewbranding.signviewtextfont() : '';


      if (!showHeader)
        return (<div/>);
      else
        return (
          <div className={"pageheader " + (this.props.fullWidth ? "full-width-header" : "")}
               style={{backgroundImage: bgImage, backgroundColor: bgColor, color: color,fontFamily : font}}>
            <div className="content">
              <div className="logowrapper">
                <img className="logo"
                     src={(signviewbranding.signviewlogo() != undefined && signviewbranding.signviewlogo() != "") ? signviewbranding.signviewlogo() : '/img/logo.png'}>
                </img>
              </div>
              {/*if*/ hasLink &&
                <div className="sender" style={{color: color,fontFamily : font}}>
                  <div className="inner clickable" onClick={this.props.link.onClick}>
                      <a className='link' style={{color: color}}>
                        {this.props.link.text}
                      </a>
                  </div>
                </div>
              }
              {/*else*/ !hasLink &&
                <div className="sender" style={{color: color,fontFamily : font}}>
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
  });

});
