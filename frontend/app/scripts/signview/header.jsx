/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixin'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      document: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      documentid: React.PropTypes.string.isRequired,
      signatorylinkid: React.PropTypes.string.isRequired,
      link : React.PropTypes.object,
      authorFullname: React.PropTypes.string,
      authorPhone: React.PropTypes.string
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.document];
    },
    logoLink : function() {
      return "/signview_logo/" + this.props.documentid + "/" + this.props.signatorylinkid + "/" + window.brandinghash;
    },
    render: function() {
      var hasLink = this.props.link != undefined;

      return (
        <div className={"header"} >
          <div className="main">
            <div className="row">
              <div className="col-xs-6 left">
                <div className="vertical left">
                  <div className="middle">
                    <img className="logo" src={this.logoLink()} />
                  </div>
                </div>
              </div>
              <div className="col-xs-6 right">
                <div className="vertical">
                  <div className="middle">
                    {/* if */ hasLink &&
                      <a className="link" onClick={this.props.link.onClick}>
                        {this.props.link.text}
                      </a>
                    }
                    {/* else */ !hasLink &&
                      <span>
                        <h4 className="info">{_.unescape(this.props.authorFullname)}</h4>
                        <h4 className="info">{_.unescape(this.props.authorPhone)}</h4>
                      </span>
                    }
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div className="clearfix" />
        </div>
    );
    }
  });

});
