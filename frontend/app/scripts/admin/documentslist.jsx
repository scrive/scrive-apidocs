/** @jsx React.DOM */

define(["React", "archive/utils", "lists/list", "legacy_code"], function(React, Utils, List) {

return React.createClass({
    mixins : [List.ReloadableContainer],
    url : function() {
      if (this.props.userid != undefined && this.props.userid!= "") {
        return "/adminonly/documentslist?userid=" + this.props.userid;
      } else if (this.props.companyid != undefined && this.props.companyid != "") {
        return "/adminonly/documentslist?companyid=" + this.props.companyid;
      } else {
        return "/adminonly/documentslist";
      }
    },
    allowSortingAndFiltering : function() {
      return (this.props.userid != undefined && this.props.userid!= "") || (this.props.companyid != undefined && this.props.companyid != "");
    },
    render: function() {
      var self = this;
      return (
        <List.List
          url={self.url()}
          dataFetcher={function(d) {return d.documents;}}
          idFetcher={function(d) {return d.field("id");}}
          totalCountFunction={function(data){ return data.total_matching;}}
          maxPageSize={100}
          paramsFunction = {function(text,selectfiltering,sorting, offset, maxPageSize) {
            var filters = [];
            if (text) {
              filters.push({"filter_by" : "text", "text" : text});
            }
            if (selectfiltering) {
              _.each(selectfiltering.filters(), function(f) {
                filters = filters.concat(f.value);
              });
            }
            var sortingBy;
            if (sorting.current()) {
              sortingBy = {
                sort_by : sorting.current(),
                order : sorting.isAsc() ? "ascending" : "descending"
              };
            } else {
              sortingBy = {
                sort_by : "mtime",
                order: "descending"
              };
            }
            return {
              filter : JSON.stringify(filters),
              sorting: JSON.stringify([sortingBy]),
              offset : offset,
              max : maxPageSize
            };
          }}
          ref='list'
        >

          { /* if */ (this.allowSortingAndFiltering()) &&
            (<List.TextFiltering text="Filter documents" />)
          }
          <List.Column
            name="Dates"
            width="80px"
            sorting={self.allowSortingAndFiltering() ? "mtime" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {d.field("ctime")}
                  <br/>
                  {d.field("mtime")}
                </small>
              );
            }}
          />

          <List.Column
            name="Author"
            width="120px"
            sorting={self.allowSortingAndFiltering() ? "author" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {Utils.signatoryName(Utils.documentAuthor(d))}
                  <br/>
                  {Utils.signatoryEmail(Utils.documentAuthor(d))}
                  <br/>
                  {Utils.signatoryCompany(Utils.documentAuthor(d))}
                </small>
              );
            }}
          />

          <List.Column
            name="Title"
            width="120px"
            sorting={self.allowSortingAndFiltering() ? "title" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {d.field("title")}
                </small>
              );
            }}
          />

          <List.Column
            name="Status"
            width="40px"
            sorting={self.allowSortingAndFiltering() ? "status" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {Utils.documentStatus(d)}
                </small>
              );
            }}
          />

          <List.Column
            name="Type"
            width="40px"
            rendering={function(d) {
              return (
                <small>
                  {d.field("is_template") ? "Template" : "Signable"}
                </small>
              );
            }}
          />
          <List.Column
            name="Signatories"
            width="120px"
            rendering={function(d) {
              return (
                <small>
                  {
                    _.map(d.field("signatories"), function(s) {
                      if (s.is_signatory) {
                        return [<span>{Utils.signatorySmartName(s)}</span>,<br/>];
                      }
                    })
                  }
                </small>
              );
            }}
          />
          { /* if */ (this.props.forAdmin) &&
            (<List.Column
              name="DAVE"
              width="30px"
              rendering={function(d) {
                return (
                  <a className="gotodave" href={"/dave/document/" + d.field("id")}/>
                );
              }}
            />)
          }
          <List.Pagination/>
        </List.List>
      );
    }
});

});
