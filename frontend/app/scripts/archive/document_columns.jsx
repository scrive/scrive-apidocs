/** @jsx React.DOM */
/* List of columns used by out archive view. Used by both, Documents and Trash list*/

define(['React','archive/utils', 'lists/list', 'moment', 'Backbone', 'legacy_code'], function(React, Utils, List, moment) {

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
                  onMouseOver={function(e) {self.showToolTip(Utils.documentStatus(d),e)}}
                  onMouseOut={function() {self.hideToolTip()}}
                  className={"icon status "+Utils.documentStatus(d)}
                />);
            }}
          />,
          <List.Column
            name={localization.archive.documents.columns.time}
            width="105px"
            sorting="mtime"
            key="time"
            rendering={function(d) {
              var time = moment(d.field("mtime")).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />,

          <List.Column
            name={localization.archive.documents.columns.sender}
            width="140px"
            sorting="author"
            key="author"
            rendering={function(d) {
              return (<a href={Utils.documentLink(d)}>{Utils.signatorySmartName(Utils.documentAuthor(d))}</a>);
            }}
          />,
          <List.Column
            name={localization.archive.documents.columns.party}
            width="210px"
            key="party"
            rendering={function(d) {
                return (<div onClick={function() {d.toggleExpand();}}>{Utils.documentParty(d)}</div>);
            }}
          />,
          <List.Column
            name={localization.archive.documents.columns.title}
            width="230px"
            key="title"
            sorting="title"
            rendering={function(d) {
              return (<a className="s-archive-document-title" href={Utils.documentLink(d)}>{d.field("title")}</a>);
            }}
          />,
          <List.Sublist
            key="signatories"
            count={function(d) {
              if (d.field("status") == "preparation") {
                return 0;
              } else {
                return _.filter(d.field("parties"), function(s) { return s.is_signatory;}).length;
              }
            }}
            rendering={function(d,i) {
              var signatory = _.filter(d.field("parties"), function(s) { return s.is_signatory;})[i];
              var time = Utils.signatoryTime(signatory) && moment(Utils.signatoryTime(signatory)).toDate();
              return [
                <td key="1"></td>,
                <td key="2">
                  <div
                    style={{"marginLeft":"10px"}}
                    className={"icon status "+ Utils.signatoryStatus(d,signatory) }
                    onMouseOver={function(e) {self.showToolTip(Utils.signatoryStatus(d,signatory),e)}}
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
                    <span>{Utils.signatorySmartName(signatory)}</span>
                  </div>
                </td>,
                <td key="6"></td>];
            }}
          />
];
}



});
