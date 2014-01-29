/** @jsx React.DOM */
// TODO(jens): Move to better place

define(['React', 'Backbone', 'imagesLoaded'], function(React, Backbone, imagesLoaded) {
  var expose = {};

  /**
   *  @description
   *  Make get functions to their corresponding values, for easier use in inline-styling
   */
  var parseBranding = function(branding) {
    var call = ['signviewbarscolour', 'signviewlogo', 'signviewtextfont', 'signviewbarstextcolour', 'fullname', 'phone'];
    _.each(call, function(functionName) {
      if(typeof(branding[functionName]) === "function") {
	branding[functionName] = branding[functionName]();
      }
    });      
    return branding;
  };
 
  expose.Header = React.createClass({
    
    /**
     *  @description
     *  Fix image/container height bug (chrome)
     *
     *  @note
     *  Since we're using a table for header, the height of it isnt correct when the image
     *  isnt loaded from the start when page render. This is a fix to set correct height of
     *  the header.
     *
     *  TODO(jens): Solve it in a more generic way, when we start using this Header everywhere.
     */
    fixPageHeaderHeight: function(rootNode) {
      imagesLoaded(jQuery(rootNode).find('img'), function(elem) {
        var imgHeight = jQuery(elem.elements[0]).height();
        
        var pageHeaderHeight = imgHeight > 90 ? imgHeight + 90 : false;
        if(pageHeaderHeight) {
          jQuery(rootNode).height(pageHeaderHeight);
        }
      });
    },
    
    componentDidMount: function(rootNode) {
      this.fixPageHeaderHeight(rootNode);
    },
     
    render: function() {
      var branding = parseBranding(this.props.branding);
      
      return (
          <div className="pageheader" style={{'background-color': branding.signviewbarscolour, display: 'block'}}>
            <div className="content">
              <div className="logowrapper"><img className="logo" src={ branding.signviewlogo } /></div>
              <div className="sender">
                <div className="inner">
                  <div style={{'font-family': branding.signviewtextfont, 'color': branding.signviewbarstextcolour}} className="name">{ branding.fullname }</div>
                  <div style={{'font-family': branding.signviewtextfont, 'color': branding.signviewbarstextcolour }} className="phone">{ branding.phone } </div>
                </div>
              </div>
              <div className="clearfix"></div>
            </div>
          </div>
      );
    }
  });
  
  expose.Footer = React.createClass({
    haveFooterBranding: function () {
      if (this.props.branding.barstextcolour || this.props.branding.textfont || this.props.branding.barscolour) {
	return true;
      }
      return false;
    },

    render: function() {
      var branding = parseBranding(this.props.branding),
      poweredBy;

      if(this.haveFooterBranding()) {
	poweredBy = (
	  <div ng-show="haveFooterBranding" className="poweredbyscrive">
	    <div className="text" style={{'font-family': branding.signviewtextfont, 'color': branding.signviewbarstextcolour}}>Powered by Scrive</div>
	  </div>
              
	  );
      } else {
	poweredBy = (
	  <div ng-hide="haveFooterBranding" className="poweredbyscrive">
	    <div className="text">Powered by</div>
	    <span className="logo"></span>
	  </div>
	);
      }
      
      return (
          <div className="pagefooter" style={{'background-color': branding.signviewbarscolour, display: 'block'}}>
	    <div className="content">
	      { poweredBy }
	      <div className="clearfix"></div>
            </div>
          </div>
      );
    }
  });
  
  
  return expose;
});
