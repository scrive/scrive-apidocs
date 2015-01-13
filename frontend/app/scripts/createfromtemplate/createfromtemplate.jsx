/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','lists/list', 'moment', 'legacy_code'], function(React, BackboneMixin, List, moment) {

return React.createClass({
    createFromTemplate : function(id) {
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
            window.location.href = "/d/"+JSON.parse(d).id;
          } catch(e) {
            LoadingDialog.close();
          }
        }
      }).send();
    },
    deliveryMethodText : function(data) {
      var dms = data.field("fields").deliveryMethods || [];
      dms = _.map(dms,function(dm) {
        if (dm == "email") {
          return capitaliseFirstLetter(localization.delivery.email);
        } else if (dm == "pad") {
          return capitaliseFirstLetter(localization.delivery.pad);
        } else if (dm == "mobile") {
          return capitaliseFirstLetter(localization.delivery.mobile);
        } else if (dm == "email_mobile") {
          return capitaliseFirstLetter(localization.delivery.email_mobile);
        } else if (dm == "api") {
          return capitaliseFirstLetter(localization.delivery.api);
        } else {
          return "";
        }
      });
      dms = _.uniq(dms);
      dms.sort();

      var text = dms[0] || "";
      for(var i =1 ; i< dms.length; i++)
        text += ", " + dms[i];
      return text;
    },
    render: function() {
      var self = this;
      return (
        <div className="create-from-template">
          <List.List
            url="/api/frontend/list?documentType=Template"
            dataFetcher={function(d) {return d.list;}}
            idFetcher={function(d) {return d.field("fields").id;}}
          >
          <List.ListHeader>
            <div className="headline">
              {localization.createFromTemplateDescription}
            </div>
          </List.ListHeader>

          <List.TextFiltering text={localization.archive.templates.search}/>

          <List.Column
              name={localization.archive.templates.columns.time}
              width="100px"
              sorting="time"
              rendering={function(d) {
                var time = moment(d.field("fields").time).toDate();
                return (<div text={time.fullTime()}>{time.toYMDString()}</div>);
              }}
            />


          <List.Column
            name={localization.archive.templates.columns.template}
            width="360px"
            sorting="title"
            rendering={function(d) {
              var id = d.field("fields").id;
              var title = d.field("fields").title;
              return (<a onClick={function(){self.createFromTemplate(id);return false;}}>{title}</a>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.verificationMethod}
            width="100px"
            rendering={function(d) {
              return (<div>{self.deliveryMethodText(d)}</div>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.shared}
            width="52px"
            rendering={function(d) {
              return (<div className={(d.field("fields").shared) ? "sharedIcon" : ""}/>);
            }}
          />

          <List.Pagination/>

          </List.List>
        </div>
      );
    }
});


});

