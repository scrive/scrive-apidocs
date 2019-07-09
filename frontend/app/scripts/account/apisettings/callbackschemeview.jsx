var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CallbackScheme = require("./callbackscheme");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");



module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    reload : function() {
      this.props.model.fetch();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      if (!model.ready) {
          return (<div/>);
      }
      return (
        <div>
          {/*if*/ (model.salesforceScheme()) &&
            <div>
              <div className='oauth-section-header'>
                {localization.integrations.salesforce}
              </div>
              <div className='oauth-section-label'>
                {localization.integrations.salesforceDescription}
              </div>
            </div>
          }
          {/*else if*/ (model.constantScheme()) &&
            <div>
              <div className='oauth-section-header'>
                {localization.integrations.constantUrlCallback}
              </div>
              <div className='oauth-section-label'>
                <HtmlTextWithSubstitution
                  secureText={localization.integrations.constantUrlCallbackDescription}
                  subs={{".put-url-here": model.constantUrl()}}
                />
              </div>
            </div>
          }
          {/*else if*/ (model.basicAuthScheme()) &&
            <div>
              <div className='oauth-section-header'>
                {localization.integrations.basicAuth}
              </div>
              <div className='oauth-section-label'>
                {localization.integrations.basicAuthDescription}
              </div>
            </div>
          }
          {/*else*/ (model.oauth2Scheme() || model.hi3gScheme() || model.emptyScheme() ) &&
            <div/>
          }
        </div>
      );
    }
  });
