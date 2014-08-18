/** @jsx React.DOM */

define(['React','common/backbone_mixin','account/apisettings/callbackscheme'], function(React, BackboneMixin, CallbackScheme) {


return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    constantUrlCallbackDescription : function() {
      var res = $("<div>" + localization.integrations.constantUrlCallbackDescription + "</div>");
      res.find('.put-url-here').text( this.props.model.constantUrl());
      return res.html();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      if (!model.ready) {
          return (<div/>)
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
              <div className='oauth-section-label'
                  dangerouslySetInnerHTML={{__html: this.constantUrlCallbackDescription()}}
              />
            </div>
          }
          {/*else*/ (model.emptyScheme() ) &&
            <div/>
          }
        </div>
      );
    }
  });
});
