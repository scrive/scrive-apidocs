/** @jsx React.DOM */

define(['React','legacy_code'],function(React) {


return React.createClass({
    propTypes: {
      className: React.PropTypes.string
    },
    className: function() {
      return (this.props.className || "");
    },
    isSelect : function() {
      return this.props.select == true;
    },
    isSortable : function() {
      return this.props.sorting != undefined;
    },
    render: function() {
      var self = this;
      if (!this.props.data) { // We render a generic header for list - since there are no data to bind
        return (
          <th className={this.className()} style={{"width" : this.props.width}}>
            {/*if*/ (this.isSelect()) &&
              (<div
                className="checkbox"
                onClick={function() {self.props.model.list().toogleSelect();}}
               />)
            }
            {/*else if*/ (!this.isSelect() && this.isSortable()) &&
              (
                <span
                  className="sortable"
                  onClick={function() {self.props.model.sorting().sortOn(self.props.sorting);}}
                >
                  {this.props.name}
                  { /*if*/ (self.props.model.sorting().isCurrent(this.props.sorting)) &&
                    (
                      <span>
                        {self.props.model.sorting().isAsc() ? " ▼" : " ▲"}
                      </span>
                    )
                  }
                </span>
              )
            }
            {/*else*/ (!this.isSelect() && !this.isSortable()) &&
              (this.props.name)
            }
          </th>);
      } else {
        return (
          <td className="row {this.className()}">
            {/*if*/ (this.isSelect()) &&
              (<div
                className={"checkbox " + (this.props.data.isSelected() ? "checked" : "")}
                onClick={function() {self.props.data.toogleSelect();}}
               />)
            }
            {/*else*/ (!this.isSelect()) &&
              (this.props.rendering(this.props.data))
            }
          </td>
        );
      }
    }
});

});

