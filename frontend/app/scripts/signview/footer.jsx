/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      document: React.PropTypes.object,
      forceShowing: React.PropTypes.bool // OVERRIDES Sign View Branding setting "showfooter"
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      if (this.props.document) {
        return [this.props.document];
      } else {
        return [];
      }
    },
    render: function() {
      var document = this.props.document;
      
      var documentUndefinedOrDocumentReady = document == undefined || document.ready();
      var documentUndefinedOrDocumentReadyWithShowFooter = document == undefined || (document.ready() && document.showfooter());

      var showFooter = documentUndefinedOrDocumentReadyWithShowFooter && !BrowserInfo.isSmallScreen();

      if (documentUndefinedOrDocumentReady && this.props.forceShowing) {
        showFooter = true;
      }

      $('.signview').toggleClass("nofooter",!showFooter); // We need to toogle this class here

      if (!showFooter)
        return (<div/>);
      else
        return (
          <div className={"pagefooter " + (BrowserInfo.isSmallScreen() ? "small-screen" : "")}>
            <div className="content">
                <div className="poweredbyscrive">
                  <div className='text' > Powered by </div>
                  <span className='logo' />
                </div>
              <div className="clearfix"/>
            </div>
          </div>
      );
    }
  });

});
