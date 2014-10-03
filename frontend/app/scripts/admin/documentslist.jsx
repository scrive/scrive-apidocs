/** @jsx React.DOM */

define(['React', 'lists/list','legacy_code'], function(React, List) {

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
          dataFetcher={function(d) {return d.list;}}
          idFetcher={function(d) {return d.field("fields").id;}}
          loadLater={self.props.loadLater}
          ref='list'
        >

          { /* if */ (this.allowSortingAndFiltering()) &&
            (<List.TextFiltering text="Filter documents" />)
          }
          <List.Column
            name="Dates"
            width="80px"
            sorting={self.allowSortingAndFiltering() ? "ctime" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {d.field("fields").ctime}
                  <br/>
                  {d.field("fields").mtime}
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
                  {d.field("fields").author.name}
                  <br/>
                  {d.field("fields").author.email}
                  <br/>
                  {d.field("fields").author.company}
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
                  {d.field("fields").title}
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
                  {d.field("fields").status}
                </small>
              );
            }}
          />

          <List.Column
            name="Type"
            width="40px"
            sorting={self.allowSortingAndFiltering() ? "type" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {d.field("fields").type}
                </small>
              );
            }}
          />
          <List.Column
            name="Signatories"
            width="120px"
            sorting={self.allowSortingAndFiltering() ? "signs" : undefined}
            rendering={function(d) {
              return (
                <small>
                  {
                    d.field("fields").signs &&
                      _.map(d.field("fields").signs, function(signatory) {
                           return [<span>{signatory}</span>,<br/>];
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
                  <a className="gotodave" href={"/dave/document/" + d.field("fields").id }/>
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
