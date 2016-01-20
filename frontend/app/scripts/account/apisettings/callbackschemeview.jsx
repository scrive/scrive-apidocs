/** @jsx React.DOM */

define(['React','common/backbone_mixin','account/apisettings/callbackscheme','common/htmltextwithsubstitution'], function(React, BackboneMixin, CallbackScheme, HtmlTextWithSubstitution) {


return React.createClass({
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
          {/*else*/ (model.oauth2Scheme() || model.emptyScheme() ) &&
            <div/>
          }
        </div>
      );
    }
  });
});
