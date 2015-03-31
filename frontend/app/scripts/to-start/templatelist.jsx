/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','lists/list', 'to-start/deliverymethodtooltip', 'legacy_code'], function(React, BackboneMixin, List, DeliveryMethodTooltipMixin) {

return React.createClass({
  mixins: [DeliveryMethodTooltipMixin],
    createFromTemplate : function(id) {
      mixpanel.track('Click on template in to-start list');
      new Submit({
        method : "POST",
        url: "/api/frontend/createfromtemplate/" +  id,
        ajax: true,
        expectedType : "text",
        onSend: function() {
          LoadingDialog.open();
        },
        ajaxerror: function(d,a){
          LoadingDialog.close();
        },
        ajaxsuccess: function(d) {
          try {
            window.location.href = "/ts/"+JSON.parse(d).id;
          } catch(e) {
            LoadingDialog.close();
          }
        }
      }).send();
    },
    deliveryMethodIcons: function(data) {
      var dms = data.field("fields").deliveryMethods || [];
      var self = this;
      uniqdms = _.uniq(dms);

      return (<div>{
        uniqdms.map(function(d) { 
          return (<div 
            className={self.icons[d]} 
            key={"deliveryMethod-" + d}
            style={{'display': 'inline-block'}} 
            onMouseOver={function(e) { self.showToolTip(d, e); }}
            onMouseOut={function(e) { self.hideToolTip(); }}
          />);
        })
      }</div>);
    },
    icons: {
      email: 'design-view-action-participant-icon-device-icon-email',
      pad: 'design-view-action-participant-icon-device-icon-pad',
      api: 'design-view-action-participant-icon-device-icon-pad',
      mobile: 'design-view-action-participant-icon-device-icon-phone',
      email_mobile : 'design-view-action-participant-icon-device-icon-email-mobile'
    },
    render: function() {
      var self = this;
      return (
        <div className="create-from-template">

          <List.List
            url="/api/frontend/list?documentType=Template"
            dataFetcher={function(d) {
              var list = d.list;
              var onlyTemplatesWithFile = _.reject(list, function(doc) { 
                return doc.fields['file'] == null;
              });

              return onlyTemplatesWithFile;
            }}
            idFetcher={function(d) {return d.field("fields").id;}}
          >

          <List.Column
            name={localization.archive.templates.columns.template}
            sorting="title"
            rendering={function(d) {
              var id = d.field("fields").id;
              var title = d.field("fields").title;
              return (<a onClick={function(){self.createFromTemplate(id);return false;}}>{title}</a>);
            }}
          />

          <List.Column
            name={localization.archive.templates.columns.verificationMethod}
            width="185px"
            rendering={function(d) {
              return (<div>{self.deliveryMethodIcons(d)}</div>);
            }}
          />

          </List.List>
        </div>
      );
    }
});


});

