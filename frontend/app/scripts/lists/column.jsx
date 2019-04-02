var React = require("react");



module.exports = React.createClass({
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
    isHidden : function() {
      return !!this.props.ishidden && this.props.ishidden(this.props.model);
    },
    render: function() {
      var self = this;
      var thstyle = {"width" : this.props.width};
      var tdstyle = {};
      if (self.isHidden()) {
        thstyle["display"] = "none";
        tdstyle["display"] = "none";
      };

      if (!this.props.data) { // We render a generic header for list - since there are no data to bind
        return (
          <th className={this.className()} style={thstyle}>
            {/*if*/ (this.isSelect()) &&
              (<div
                className="checkbox"
                onClick={function() {self.props.model.list().toogleSelect();}}
               >
                 <div className="checkmark" />
               </div>
               )
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
          <td className={"row " + this.className()} style={tdstyle}>
            {/*if*/ (this.isSelect()) &&
              (<div
                className={"checkbox " + (this.props.data.isSelected() ? "checked" : "")}
                onClick={function() {self.props.data.toogleSelect();}}
               >
                <div className="checkmark" />
              </div>)
            }
            {/*else*/ (!this.isSelect()) &&
              (this.props.rendering(this.props.data))
            }
          </td>
        );
      }
    }
});
