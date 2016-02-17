var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Utils = require("../archive/utils");
var List = require("../lists/list");
var moment = require("moment");
var Submit = require("../../js/submits.js").Submit;
var LoadingDialog = require("../../js/loading.js").LoadingDialog;

require("../../js/utils/time.js");

module.exports = React.createClass({
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
    render: function() {
      var self = this;
      return (
        <div className="create-from-template">
          <List.List
            maxPageSize={Utils.maxPageSize}
            totalCountFunction={Utils.totalCountFunction}
            url={Utils.listCallUrl}
            paramsFunction={Utils.paramsFunctionWithFilter([
                {"filter_by" : "template", "is_template" : true},
                {"filter_by" : "trash", "is_trashed" : false}
              ])}
            dataFetcher={Utils.dataFetcher}
            idFetcher={Utils.idFetcher}
            loadLater={self.props.loadLater}
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
              sorting="mtime"
              rendering={function(d) {
                var time = moment(d.field("mtime")).toDate();
                return (<div text={time.fullTime()}>{time.toYMDString()}</div>);
              }}
            />


          <List.Column
            name={localization.archive.templates.columns.template}
            width="310px"
            sorting="title"
            rendering={function(d) {
              var id = d.field("id");
              var title = d.field("title");
              return (<a onClick={function(){self.createFromTemplate(id);return false;}}>{title}</a>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.verificationMethod}
            width="100px"
            rendering={function(d) {
              return (<div>{Utils.documentDeliveryText(d)}</div>);
            }}
          />
          <List.Column
            name={localization.archive.templates.columns.sharedBy}
            width="102px"
            rendering={function(d) {
              return (<div>{(d.field("is_shared")) ? Utils.signatorySmartName(Utils.documentAuthor(d)) : ""}</div>);
            }}
          />

          <List.Pagination/>

          </List.List>
        </div>
      );
    }
});
