var React = require("react");
var Utils = require("../archive/utils");
var List = require("../lists/list");
var moment = require("moment");
var _ = require("underscore");

module.exports = React.createClass({
    mixins: [List.ReloadableContainer],
    url: function () {
      if (this.props.userid != undefined && this.props.userid != "") {
        return "/adminonly/documentslist?userid=" + this.props.userid;
      } else if (this.props.companyid != undefined && this.props.companyid != "") {
        return "/adminonly/documentslist?companyid=" + this.props.companyid;
      } else {
        return "/adminonly/documentslist";
      }
    },
    allowSortingAndFiltering: function () {
      return (this.props.userid != undefined && this.props.userid != "") ||
             (this.props.companyid != undefined && this.props.companyid != "");
    },
    render: function () {
      var self = this;
      return (
        <List.List
          url={self.url()}
          dataFetcher={function (d) { return d.documents; }}
          idFetcher={function (d) { return d.field("id"); }}
          totalCountFunction={function (data) { return data.total_matching; }}
          maxPageSize={100}
          loadLater={self.props.loadLater}
          paramsFunction = {function (text, selectfiltering, sorting, offset, maxPageSize) {
            var filters = [{"filter_by": "is_template"}];
            if (text) {
              filters.push({"filter_by": "text", "text": text});
            }
            if (selectfiltering) {
              _.each(selectfiltering.filters(), function (f) {
                filters = filters.concat(f.value);
              });
            }
            var sortingBy;
            if (sorting.current()) {
              sortingBy = {
                sort_by: sorting.current(),
                order: sorting.isAsc() ? "ascending" : "descending"
              };
            } else {
              sortingBy = {
                sort_by: "mtime",
                order: "descending"
              };
            }
            return {
              filter: JSON.stringify(filters),
              sorting: JSON.stringify([sortingBy]),
              offset: offset,
              max: maxPageSize
            };
          }}
          ref='list'
        >

          { /* if */ (this.allowSortingAndFiltering()) &&
            (<List.TextFiltering text="Filter templates" />)
          }
          <List.Column
            name="Last event"
            width="105px"
            sorting="mtime"
            rendering={function (d) {
              var time = moment(d.field("mtime")).toDate();
              return (<span title={time.fullTime()}>{time.toTimeAbrev()}</span>);
            }}
          />

          <List.Column
            name="Title"
            width="300px"
            sorting={self.allowSortingAndFiltering() ? "title" : undefined}
            rendering={function (d) {
              return (
                <span>
                  {d.field("title")}
                </span>
              );
            }}
          />

          <List.Column
            name="Owner"
            width="200px"
            sorting={self.allowSortingAndFiltering() ? "author" : undefined}
            rendering={function (d) {
              return (
                <span>
                  {Utils.signatoryName(Utils.documentAuthor(d))}
                  <br/>
                  {Utils.signatoryEmail(Utils.documentAuthor(d))}
                </span>
              );
            }}
          />

          <List.Column
            name="Shareable Link"
            width="50px"
            className="archive-table-shareablelink-column-header"
            rendering={function (d) {
              return (<div className={"archive-table-shareablelink-column " + ((d.field("shareable_link")) ? "sharedIcon" : "")}/>);
            }}
          />

          <List.Column
            name="Shared"
            width="50px"
            className="archive-table-shared-column-header"
            rendering={function (d) {
              return (<div className={"archive-table-shared-column " + ((d.field("is_shared")) ? "sharedIcon" : "")}/>);
            }}
          />
          { /* if */ (this.props.forAdmin) &&
            (<List.Column
              name="DAVE"
              width="30px"
              rendering={function (d) {
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
