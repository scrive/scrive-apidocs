/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin', 'tinycolor'], function(React, Backbone, BackboneMixin, tinycolor) {

  return React.createClass({
    propTypes: {
      document: React.PropTypes.object,
      fullWidth : React.PropTypes.bool,
      link : React.PropTypes.object,
      authorFullname: React.PropTypes.string,
      authorPhone: React.PropTypes.string,
      forceShowing: React.PropTypes.bool // Overrides sign view branding setting "showheader"
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      if (this.props.document) {
        return [this.props.document];
      } else {
        return [];
      }
    },
    logoLink : function() {
      if (this.props.documentid && this.props.signatorylinkid) {
        return "/signview_logo/" + this.props.documentid + "/" + this.props.signatorylinkid + "/" + window.brandinghash;
      } else {
        return "/signview_logo_without_document/" + window.brandinghash;
      }
    },
    render: function() {
      var document = this.props.document;
      var documentUndefinedOrDocumentReady = document == undefined || document.ready();
      var documentUndefinedOrDocumentReadyWithShowFooter = document == undefined || (document.ready() && document.showfooter());

      var showHeader = documentUndefinedOrDocumentReadyWithShowFooter && !BrowserInfo.isSmallScreen();

      if (documentUndefinedOrDocumentReady && this.props.forceShowing) {
        showHeader = true;
      }
      var hasLink = this.props.link != undefined;

      $('.signview').toggleClass("noheader",!showHeader); // We need to toogle this class here
      if (!showHeader)
        return (<div/>);
      else {
        return (
          <div className={"pageheader " + (this.props.fullWidth ? "full-width-header" : "")} >
            <div className="content">
              <div className="logowrapper">
                <img className="logo"
                     src={this.logoLink()}>
                </img>
              </div>
              {/*if*/ hasLink &&
                <div className="sender">
                  <div className="inner clickable" onClick={this.props.link.onClick}>
                      <a className='link'>
                        {this.props.link.text}
                      </a>
                  </div>
                </div>
              }
              {/*else*/ !hasLink &&
                <div className="sender">
                  <div className="inner">
                    <div className='name'>
                      {this.props.authorFullname}
                    </div>
                    <div className='phone'>
                      {this.props.authorPhone}
                    </div>
                  </div>
                </div>
              }
              <div className="clearfix"/>
            </div>
          </div>
      );
      }
    }
  });

});
