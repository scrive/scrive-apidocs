/** @jsx React.DOM */


define(['React', 'Backbone'], function(React, Backbone) {
  var expose = {};
  
  expose.BrandedBanner = React.createClass({
    propTypes: {
      registerUser: React.PropTypes.func.isRequired,
    },

    render: function() {
      var containerDivStyle = {
        'height': '368px',
        'background-image': 'url(' + this.props.promotionImg + ')'
      };
      
      var buttonStyle = {
        'background-color': 'rgb(162, 198, 23)',
        'margin-top': '300px',
        'margin-left': '404px',
        'box-shadow': 'none',
        'border': 'none',
        'border-top-right-radius': '3px',
        'border-top-left-radius': '3px',
        'border-bottom-right-radius': '3px',
        'border-bottom-left-radius': '3px',
        'background-position': 'initial initial',
        'background-repeat': 'initial initial'
      };
      
      var labelStyle = {
        'font-size': '19px',
        'font-weight': 'bold'
      };
      
      return (
          React.DOM.div( {className:"save", style:containerDivStyle}, 
            React.DOM.div(null),
            React.DOM.a( {onClick:this.props.registerUser, style:buttonStyle, className:"green button button-large button-green"}, 
              React.DOM.div( {className:"label", style:labelStyle},  localization.docSignView.promoBannerButton )
            )
          )
      );
    }
  });
  
  expose.SaveBackupCopy = React.createClass({
    propTypes: {
      registerUser: React.PropTypes.func.isRequired,
      isSmallScreen: React.PropTypes.bool.isRequired
    },
    
    render: function() {
      // TODO(jens): Remove inline styling
      var labelStyle = {
        'display': 'block',
        'padding-bottom': '5px'
      };
      
      var acceptButtonStyle = {
        'margin-left': '12px'
      };
      
      var cx = React.addons.classSet;
      var mainContainerClasses = cx({
        'small-screen': this.props.isSmallScreen,
        'save': true,
        'section': true,
        'spacing': true
      });
      
      var titles;
      if(!this.props.isSmallScreen) {
        titles = (
            React.DOM.div(null, 
              React.DOM.div( {className:"title"},  localization.docSignView.titleText ),
              React.DOM.div( {className:"subtitle"},  localization.docSignView.subtitleText )
            )
        )
      } else {
        titles = (
            React.DOM.div( {className:"title"},  docSignView.subtitleText )
        )
      }
      
      return (
          React.DOM.div(null, 
            React.DOM.div( {className:mainContainerClasses}, 
               titles, 
              React.DOM.div( {class:"clearfix"}),
              React.DOM.div( {class:"acceptbutton", style:acceptButtonStyle}, 
                React.DOM.label( {style:labelStyle}, 
                  " Jag har läst och accepterat ", React.DOM.a( {target:"_blank", class:"clickable", href:"/sv/terms"}, "Scrive Allmänna Villkor")
                ),
                React.DOM.a( {onClick:this.props.registerUser, className:"green button button-large button-green button-round"}, 
                  React.DOM.div( {className:"label"},  localization.docSignView.signupButtonText )
                )
              )
            )
          )
      )
    }
  });
  
  expose.PadSafetyCopySaved = React.createClass({
    render: function() {
      return (
          React.DOM.div(null, 
            React.DOM.div( {className:"save"}, 
              React.DOM.div( {className:"title"},  localization.docSignView.padTitle ),
              React.DOM.div( {className:"subtitle"},  localization.docSignView.padSubtitle )
            )
          )
      )
    }
  });
    
  return expose;
});
