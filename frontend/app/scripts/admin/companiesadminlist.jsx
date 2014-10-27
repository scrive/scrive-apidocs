/** @jsx React.DOM */

define(['React', 'lists/list','legacy_code'], function(React, List) {


return React.createClass({
     mixins : [List.ReloadableContainer],
    render: function() {
      var self = this;
      return (
        <List.List
          url='/adminonly/companies'
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >

          <List.TextFiltering text={"Search companies"} />

          <List.SelectFilter
            name="users"
            textWidth="173px"
            options={[
              {name: "With more then one user", value: ""},
              {name: "All", value: "all"}
            ]}
          />
          <List.Column
            name="Name"
            width="120px"
            sorting="companyname"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").companyname}</a>);
            }}
          />
          <List.Column
            name="Number"
            width="100px"
            sorting="companynumber"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").companynumber}</a>);
            }}
          />

          <List.Column
            name="Address"
            width="100px"
            sorting="companyaddress"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").companyaddress}</a>);
            }}
          />
          <List.Column
            name="Zip"
            width="50px"
            sorting="companyzip"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").companyzip}</a>);
            }}
          />
          <List.Column
            name="City"
            width="100px"
            sorting="companycity"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").companycity}</a>);
            }}
          />
          <List.Column
            name="Country"
            width="100px"
            sorting="companycountry"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").companycountry}</a>);
            }}
          />
          <List.Column
            name="ID"
            width="100px"
            rendering={function(d) {
              return (<a href={d.field("link")}>{d.field("fields").id}</a>);
            }}
          />

          <List.Pagination/>
        </List.List>
      );
    }
});

});
