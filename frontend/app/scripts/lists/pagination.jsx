var React = require("react");


module.exports = React.createClass({
    propTypes: {
      maxNextPages : React.PropTypes.number,
      showOnlyForMultiplePages : React.PropTypes.bool
    },
    getDefaultProps: function() {
      return {
        maxNextPages: 9
      };
    },
    hasManyPages : function() {
            return this.pageSize() <= this.itemMax();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var offset = model.offset();
      var pageSize = model.maxPageSize();
      var total = model.totalCount();
      var pageCurrent = Math.floor(offset / pageSize);
      var maxPage = Math.ceil(total / pageSize);
      var pages = [];
      for(var i=0; i <  maxPage && i - pageCurrent < this.props.maxNextPages; i++) {
          pages.push(i);
      }
      return (
        <div className='table-paginate'>
          <div className='pages'>
            { /*if*/ (this.props.showOnlyForMultiplePages != true || total > pageSize ) &&
              (
                <span>
                { pages.map(function(i) {
                    return (
                      <span
                        key={i}
                        className={"page-change " + (i == pageCurrent ? "current" : "")}
                        onClick={function() { model.changeOffsetAndReload(i * pageSize);}}
                      >
                        {i+1}
                      </span>
                    );
                  })
                }
                { /*if*/ (pages.length * pageSize < total) &&
                  (<span
                    className={"page-change " + (i == pageCurrent ? "current" : "")}
                    onClick={function() { model.changeOffsetAndReload(pages.length * pageSize)}}
                  >
                    &gt;
                  </span>)
                }
                </span>
              )
            }
          </div>
        </div>
      );
    }
  });
