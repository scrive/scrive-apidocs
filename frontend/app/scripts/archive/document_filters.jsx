/** @jsx React.DOM */
/* List of filters used by out archive view. Used by both, Documents and Trash list*/

define(['React','lists/list','Backbone', 'legacy_code'], function(React,List) {

return function(args) {
  var self = args.list;
  return [
    <List.SelectFilter
            key="status"
            name="status"
            textWidth={135}
            options = {[
              {name: localization.filterByStatus.showAnyStatus, value: ""},
              {name: localization.filterByStatus.showDraft,     value: "[draft]"},
              {name: localization.filterByStatus.showCancelled, value: "[cancelled,rejected,timeouted,problem]"},
              {name: localization.filterByStatus.showSent,      value: "[sent,delivered,read,opened,deliveryproblem]"},
              {name: localization.filterByStatus.showSigned,    value: "[signed]"}
            ]}
   />,
   self.props.forCompanyAdmin &&
   (<List.SelectAjaxFilter
     name="sender"
     key="sender"
     textWidth={135}
     defaultName={localization.filterByAuthor.showAnyAuthor}
     optionsURL="/companyaccounts"
     optionsParse={function(resp) {
       var options = [];
       _.each(resp.list, function(l) {
         var fields = l.fields;
         var id = fields["id"];
         var name = fields["fullname"];
         if (name == undefined || name.trim() == "")
           name = fields["email"];
         if (fields["activated"])
           options.push({name : name , value : id });
         });
         return options;
      }}
   />),
   <List.SelectFromToFilter
     key="time"
     name="time"
     textWidth={100}
     fromText={localization.filterByTime.filterForm}
     toText= {localization.filterByTime.filterTo}
     options={
       (function() {
          var time = new Date();
          var year = self.props.year;
          var month = self.props.month;
          var options = [];
          while (year < time.getFullYear() || (year == time.getFullYear() && month <= time.getMonth() + 1)) {
            var name = capitaliseFirstLetter(localization.months[month-1].slice(0,3) + " " + year);
            options.push({name : name , value : "("+month + "," + year + ")" });
            month++;
            if (month == 13) {month = 1; year++;}
          }
          return options;
       }())
     }
   />

  ];
}



});
