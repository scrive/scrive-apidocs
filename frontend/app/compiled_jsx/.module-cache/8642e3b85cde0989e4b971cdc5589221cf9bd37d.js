/** @jsx React.DOM */
// TODO(jens): Move to better place
a
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

    /**
     *  @description
     *  Return logo to show
     */
    getLogo: function(signviewlogo) {
      if(typeof(signviewlogo) === 'string') {
	  return branding.signviewlogo
      } else {
	return '/img/logo.png';
      }
    },
    
    componentDidMount: function(rootNode) {
      this.fixPageHeaderHeight(rootNode);
    },
     
    render: function() {
      var branding = parseBranding(this.props.branding);

      return (
          React.DOM.div( {className:"pageheader", style:{'background-color': branding.signviewbarscolour, display: 'block'}}, 
            React.DOM.div( {className:"content"}, 
              React.DOM.div( {className:"logowrapper"}, React.DOM.img( {className:"logo", src: this.getLogo(branding.signviewlogo) } )),
              React.DOM.div( {className:"sender"}, 
                React.DOM.div( {className:"inner"}, 
                  React.DOM.div( {style:{'font-family': branding.signviewtextfont, 'color': branding.signviewbarstextcolour}, className:"name"},  branding.fullname ),
                  React.DOM.div( {style:{'font-family': branding.signviewtextfont, 'color': branding.signviewbarstextcolour }, className:"phone"},  branding.phone  )
                )
              ),
              React.DOM.div( {className:"clearfix"})
            )
          )
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
	  React.DOM.div( {'ng-show':"haveFooterBranding", className:"poweredbyscrive"}, 
	    React.DOM.div( {className:"text", style:{'font-family': branding.signviewtextfont, 'color': branding.signviewbarstextcolour}}, "Powered by Scrive")
	  )
              
	  );
      } else {
	poweredBy = (
	  React.DOM.div( {'ng-hide':"haveFooterBranding", className:"poweredbyscrive"}, 
	    React.DOM.div( {className:"text"}, "Powered by"),
	    React.DOM.span( {className:"logo"})
	  )
	);
      }
      
      return (
          React.DOM.div( {className:"pagefooter", style:{'background-color': branding.signviewbarscolour, display: 'block'}}, 
	    React.DOM.div( {className:"content"}, 
	       poweredBy, 
	      React.DOM.div( {className:"clearfix"})
            )
          )
      );
    }
  });
  
  
  return expose;
});
