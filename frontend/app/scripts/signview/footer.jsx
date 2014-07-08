/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixim'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.signviewbranding
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.signviewbranding];
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      var showFooter = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showfooter();

      $('.signview').toggleClass("nofooter",!showFooter); // We need to toogle this class here

      var bgImage = signviewbranding.signviewbarscolour() != undefined ?  'none' : '';
      var bgColor = signviewbranding.signviewbarscolour() != undefined ?  signviewbranding.signviewbarscolour() : '';
      var color = signviewbranding.signviewbarstextcolour() != undefined ? signviewbranding.signviewbarstextcolour() : '';
      var font = signviewbranding.signviewtextfont() != undefined ? signviewbranding.signviewtextfont() : '';


      if (!showFooter)
        return (<div/>);
      else
        return (
          <div className={"pagefooter " + (BrowserInfo.isSmallScreen() ? "small-screen" : "")}
               style={{backgroundImage: bgImage, backgroundColor: bgColor, color: color,fontFamily : font}}>
            <div className="content">
              {/*if*/ signviewbranding.signviewbarscolour() &&
                <div className="poweredbyscrive">
                  <span className='text' >
                    Powered by Scrive
                  </span>
                </div>
              }
              {/*else*/ !signviewbranding.signviewbarscolour() &&
                <div className="poweredbyscrive">
                  <div className='text' > Powered by </div>
                  <span className='logo' />
                </div>
              }
              <div className="clearfix"/>
            </div>
          </div>
      );
    }
  });

});
