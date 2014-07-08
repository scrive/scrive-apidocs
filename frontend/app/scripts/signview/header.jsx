/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixim'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.signviewbranding,
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
