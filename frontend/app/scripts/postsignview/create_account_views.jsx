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
          <div className="save" style={containerDivStyle}>
            <div></div>
            <a onClick={this.props.registerUser} style={buttonStyle} className="green button button-large button-green">
              <div className="label" style={labelStyle}>{ localization.docsignview.promoBannerButton }</div>
            </a>
          </div>
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
            <div>
              <div className="title">{ localization.docsignview.titleText }</div>
              <div className="subtitle">{ localization.docsignview.subtitleText }</div>
            </div>
        )
      } else {
        titles = (
            <div className="title">{ localization.docsignview.subtitleText }</div>
        )
      }
      
      return (
          <div>
            <div className={mainContainerClasses}>
              { titles }
              <div class="clearfix"></div>
              <div class="acceptbutton" style={acceptButtonStyle}>
                <label style={labelStyle}>
                  Jag har läst och accepterat <a target='_blank' class='clickable' href='/sv/terms'>Scrive Allmänna Villkor</a>
                </label>
                <a onClick={this.props.registerUser} className="green button button-large button-green button-round">
                  <div className="label">{ localization.docsignview.signupButtonText }</div>
                </a>
              </div>
            </div>
          </div>
      )
    }
  });
  
  expose.PadSafetyCopySaved = React.createClass({
    render: function() {
      return (
          <div>
            <div className="save">
              <div className="title">{ localization.docsignview.padTitle }</div>
              <div className="subtitle">{ localization.docsignview.padSubtitle }</div>
            </div>
          </div>
      )
    }
  });
    
  return expose;
});
