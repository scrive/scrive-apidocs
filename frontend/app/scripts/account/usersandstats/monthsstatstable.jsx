/** @jsx React.DOM */

define(['React', 'lists/list','legacy_code'], function(React, List) {

return React.createClass({
    mixins : [List.ReloadableContainer],
    render: function() {
      var self = this;
      return (
          <List.List
            url={self.props.url}
            dataFetcher={function(d) {return d.list;}}
          >

            <List.Column
              name={localization.account.stats.columnMonth}
              width="100px"
              rendering={function(d) {
                return (<div>{d.field("fields").date}</div>);
              }}
            />
            {/*if*/ (self.props.withCompany) &&
              (
                <List.Column
                  name={localization.account.stats.columnSender}
                  width="100px"
                  className="sender"
                  rendering={function(d) {
                    return (<div onClick={function() {d.toggleExpand();}}>{d.field("fields").name}</div>);
                  }}
                />
             )
            }

            {/*if*/ (self.props.withCompany) &&
              (
                <List.Sublist
                  count={function(d) {return d.field("subfields") != undefined ? d.field("subfields").length : 0}}
                  rendering={function(d,i) {
                    return [
                      <td key="1"><div style={{"marginLeft":"10px"}}>{d.field("subfields")[i].date}</div></td>,
                      <td key="2"><div style={{"marginLeft":"10px"}}>{d.field("subfields")[i].name}</div></td>,
                      <td key="3"><div style={{"marginLeft":"10px"}}>{d.field("subfields")[i].closed}</div></td>,
                      <td key="4"><div style={{"marginLeft":"10px"}}>{d.field("subfields")[i].sent}</div></td>,
                      <td key="5"><div style={{"marginLeft":"10px"}}>{d.field("subfields")[i].signatures}</div></td>
                    ];
                   }}
                />
              )
            }

            <List.Column
              name={localization.account.stats.columnClosedDocuments}
              width="70px"
              rendering={function(d) {
                return (<div>{d.field("fields").closed}</div>);
              }}
            />

            <List.Column
              name={localization.account.stats.columnSender}
              width="70px"
              rendering={function(d) {
                return (<div>{d.field("fields").sent}</div>);
              }}
            />

            <List.Column
              name={localization.account.stats.columnSender}
              width="70"
              rendering={function(d) {
                return (<div>{d.field("fields").signatures}</div>);
              }}
            />

          </List.List>
      );
    }
});


});

