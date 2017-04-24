var React = require("react");
var List = require("../lists/list");
var _ = require("underscore");
/* List of filters used by out archive view. Used by both, Documents and Trash list*/

var SELECT_STATUS_OPTIONS = [
  {
    name: localization.filterByStatus.showAnyStatus,
    value: []
  },
  {
    name: localization.filterByStatus.showDraft,
    value: [{"filter_by": "status", "statuses": ["preparation"]}]
  },
  {
    name: localization.filterByStatus.showCancelled,
    value: [
      {"filter_by": "status", "statuses": ["canceled", "timedout", "rejected", "document_error"]}
    ]
  },
  {
    name: localization.filterByStatus.showSent,
    value: [{"filter_by": "status", "statuses": ["pending"]}]
  },
  {
    name: localization.filterByStatus.showSigned,
    value: [{"filter_by": "status", "statuses": ["closed"]}]
  }
];


module.exports = function(args) {
  var self = args.list;
  var fromToFilterOptions = args.fromToFilterOptions;

  return [
    <List.SelectFilter
      key="status"
      name="status"
      width={167}
      options = {SELECT_STATUS_OPTIONS}
   />,
   self.props.forCompanyAdmin &&
   (<List.SelectAjaxFilter
     name="sender"
     key="sender"
     width={167}
     defaultOption={{name : localization.filterByAuthor.showAnyAuthor, value: []}}
     optionsURL="/companyaccounts"
     optionsParse={function(resp) {
       var options = [];
       _.each(resp.accounts, function(a) {
         var name = a.fullname;
         if (name == undefined || name.trim() == "")
           name = a.email;
         if (a.activated)
           options.push({name : name , value : [{"filter_by" :"author", "user_id" : a.id}] });
         });
         return options;
      }}
   />),
   <List.SelectFromToFilter
     key="time"
     name="time"
     width={127}
     fromText={localization.filterByTime.filterForm}
     toText= {localization.filterByTime.filterTo}
     emptyValue={[]}
     options={fromToFilterOptions}
   />

  ];
}
