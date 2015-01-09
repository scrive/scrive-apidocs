/** @jsx React.DOM */

define(['React', 'Backbone'], function(React, Backbone) {
  var expose = {};

  expose.BrandedBanner = React.createClass({
    propTypes: {
      registerUser: React.PropTypes.func.isRequired
    },

    render: function() {
      var containerClasses = ['save',
                              'branded-banner',
                              this.props.bannerType,
                              this.props.language].join(' ');
      return (
          <div className={ containerClasses }>
            <div></div>
            <a onClick={this.props.registerUser} className="green button button-large action">
              <div className="label">{ localization.docsignview.promoBannerButton }</div>
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
    makeTOSCopyWithLink: function() {
      var res = $("<span>" + localization.docsignview.acceptTOS + "</span>");
      $('.is-TOS',res)
        .addClass('terms clickable')
        .attr('target','_blank')
        .attr('href','/terms');
      return res.html();
    },

    render: function() {
      var cx = React.addons.classSet;
      var mainContainerClasses = cx({
        'small-screen': this.props.isSmallScreen,
        'save-backup-copy': true,
        'save': true,
        'section': true,
        'spacing': true
      });

      return (
          <div>
            <div className={mainContainerClasses}>

              { /*if*/ !this.props.isSmallScreen &&
              <div>
                <div className="title">{ localization.docsignview.titleText }</div>
                <div className="subtitle">{ localization.docsignview.subtitleText }</div>
              </div>
              }
              { /*else*/ this.props.isSmallScreen &&
              <div className="title">{ localization.docsignview.subtitleText }</div>
              }

              <div className="clearfix"></div>
              <div className="acceptbutton">
                <label className="label" dangerouslySetInnerHTML={{__html: this.makeTOSCopyWithLink()}} />
                <a onClick={this.props.registerUser} className="green button button-large action">
                  <div className="label">{ localization.docsignview.signupButtonText }</div>
                </a>
              </div>
            </div>
          </div>
      );
    }
  });

  return expose;
});
