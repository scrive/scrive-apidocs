var React = require("react");
var List = require("../lists/list");
var _ = require("underscore");


var companyLimit = 100;

module.exports = React.createClass({
    mixins : [List.ReloadableContainer],
    companyLink : function(d) {
      return "/adminonly/companyadmin/" + d.field("id");
    },
    render: function() {
      var self = this;
      return (
        <List.List
          url='/adminonly/companies'
          dataFetcher={function(d) {return _.first(d.companies,companyLimit);}}
          totalCountFunction={function(data,list_offset){ return data.companies.length + list_offset;}}
          idFetcher={function(d) {return d.field("id");}}
          loadLater={self.props.loadLater}
          paramsFunction = {function(text,selectfiltering,_sorting,offset) {
            var params  = {
              // Limit is set to more than amount of companies we are displaing. This way we can get information if there are more pages.
              limit: companyLimit + 1,
              offset: offset,
              allCompanies: "true",
              nonFree: "true"
            };
            if (text) {
              params.text = text;
            }
            if (selectfiltering) {
              _.each(selectfiltering.filters(), function(f) {
                if (f.value === "allCompanies") {
                  params.nonFree = undefined;
                } else if (f.value === "moreThanOneUser") {
                  params.allCompanies = undefined;
                  params.nonFree = undefined;
                }
              });
            }
            return params;
          }}
          ref='list'
        >

          <List.TextFiltering text={"Search companies"} />

          <List.SelectFilter
            name="users"
            width={200}
            options={[
              {name: "With non-free price plan", value: ""},
              {name: "With more than one user", value: "moreThanOneUser"},
              {name: "All", value: "allCompanies"}
            ]}
          />
          <List.Column
            name="Name"
            width="120px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companyname")}</a>);
            }}
          />
          <List.Column
            name="Entity Name"
            width="100px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companyentityname")}</a>);
            }}
          />
          <List.Column
            name="Number"
            width="100px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companynumber")}</a>);
            }}
          />
          <List.Column
            name="Address"
            width="100px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companyaddress")}</a>);
            }}
          />
          <List.Column
            name="Zip"
            width="50px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companyzip")}</a>);
            }}
          />
          <List.Column
            name="City"
            width="100px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companycity")}</a>);
            }}
          />
          <List.Column
            name="Country"
            width="100px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("companycountry")}</a>);
            }}
          />
          <List.Column
            name="ID"
            width="110px"
            rendering={function(d) {
              return (<a href={self.companyLink(d)}>{d.field("id")}</a>);
            }}
          />

          <List.Pagination maxNextPages={1}/>
        </List.List>
      );
    }
});
