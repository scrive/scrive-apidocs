/** @jsx React.DOM */

define(['React','legacy_code'], function(React) {

return React.createClass({
    propTypes: {
      showOnlyForMultiplePages : React.PropTypes.bool
    },
    hasManyPages : function() {
            return this.pageSize() <= this.itemMax();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var maxNextPages = model.maxNextPages();
      var maxPage = model.pageCurrent() + maxNextPages - 1;
      var pages = [];
      for(var i=0;i < maxPage && i*model.pageSize() <= model.itemMax();i++)
          pages.push(i);

      return (
        <div className='table-paginate'>
          <div className='pages'>
            { /*if*/ (this.props.showOnlyForMultiplePages != true || model.itemMax() > model.pageSize() ) &&
              (
                <span>
                { pages.map(function(i) {
                    return (
                      <span
                        key={i}
                        className={"page-change " + (i == model.pageCurrent() ? "current" : "")}
                        onClick={function() { model.changePage(i)}}
                      >
                        {i+1}
                      </span>
                    );
                  })
                }
                { /*if*/ (maxPage*model.pageSize() < model.itemMax()) &&
                  (<span
                    className={"page-change " + (i == model.pageCurrent() ? "current" : "")}
                    onClick={function() { model.changePage(maxPage)}}
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
});

