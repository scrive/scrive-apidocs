/** @jsx React.DOM */

define(['React', 'Backbone', 'common/button'], function(React, Backbone, Button) {
  var expose = {};

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
        <div className="section safety-copy">
          <div className="col-xs-6">
            <h1>{localization.docsignview.titleText}</h1>
            <p>{localization.docsignview.subtitleText}</p>
          </div>
          <div className="col-xs-6 right">
            <Button
              type="action"
              text={localization.docsignview.signupButtonText}
              onClick={this.props.registerUser}
            />
            <p className="bottom-label" dangerouslySetInnerHTML={{__html: this.makeTOSCopyWithLink()}} />
          </div>
        </div>
      );
    }
  });

  return expose;
});
