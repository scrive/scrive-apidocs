/** @jsx React.DOM */
/* List of columns used by out archive view. Used by both, Documents and Trash list*/

define(['React','lists/list', 'moment', 'Backbone', 'legacy_code'], function(React, List, moment) {

return function(args) {
  var self = args.list;
  return [
          <List.Column
            select={true}
            width="30px"
            key="checkbox"
          />,
          <List.Column
            name={localization.archive.documents.columns.status}
            width="62px"
            sorting="status"
            key="status"
            rendering={function(d) {
              return (
                <div
                  onMouseOver={function(e) {self.showToolTip(d.field("fields").status,e)}}
                  onMouseOut={function() {self.hideToolTip()}}
                  className={"icon status "+d.field("fields").status}
                />);
            }}
          />,
          <List.Column
            name={localization.archive.documents.columns.time}
            width="105px"
            sorting="time"
            key="time"
            rendering={function(d) {
              var time = moment(d.field("fields").time).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />,

          <List.Column
            name={localization.archive.documents.columns.sender}
            width="140px"
            sorting="author"
            key="author"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").author}</a>);
            }}
          />,
          <List.Column
            name={localization.archive.documents.columns.party}
            width="210px"
            key="party"
            sorting="party"
            rendering={function(d) {
                return (<div onClick={function() {d.toggleExpand();}}>{d.field("fields").party}</div>);
            }}
          />,
          <List.Column
            name={localization.archive.documents.columns.title}
            width="230px"
            key="title"
            sorting="title"
            rendering={function(d) {
              return (<a className="s-archive-document-title" href={d.field("link")}>{d.field("fields").title}</a>);
            }}
          />,
          <List.Sublist
            key="signatories"
            count={function(d) {return d.field("subfields") != undefined ? d.field("subfields").length : 0}}
            rendering={function(d,i) {
              var time;
              if (d.field("subfields")[i].time)
                time = moment(d.field("subfields")[i].time).toDate();
              return [
                <td key="1"></td>,
                <td key="2">
                  <div
                    style={{"marginLeft":"10px"}}
                    className={"icon status "+d.field("subfields")[i].status}
                    onMouseOver={function(e) {self.showToolTip(d.field("subfields")[i].status,e)}}
                    onMouseOut={function() {self.hideToolTip()}}
                  />
                </td>,
                <td key="3">
                  { /* if */ (time != undefined) &&
                    (<div style={{"marginLeft":"10px"}}>
                      <span title={time.fullTime()}>{time.toTimeAbrev()}</span>
                     </div>)
                  }
                </td>,
                <td key="4"></td>,
                <td key="5">
                  <div style={{"marginLeft":"10px"}}>
                    <span>{d.field("subfields")[i].name}</span>
                  </div>
                </td>,
                <td key="6"></td>];
            }}
          />
];
}



});
