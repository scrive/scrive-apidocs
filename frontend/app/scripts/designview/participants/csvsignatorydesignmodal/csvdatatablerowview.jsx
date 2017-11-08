var React = require("react");
var _ = require("underscore");

var CSVDataTableCellView = require("./csvdatatablecellview");
var CSVSignatoryDesignModel = require("./csvsignatorydesignmodel");

var CSVDataTableRowView = React.createClass({
  propTypes: {
    index: React.PropTypes.number.isRequired,
    model: React.PropTypes.instanceOf(CSVSignatoryDesignModel).isRequired,
    row: React.PropTypes.array.isRequired
  },
  render: function () {
    var rowProblem = this.props.model.problemWithRow(this.props.index);
    var self = this;

    return (
      <tr>
        {_.map(_.range(0, this.props.model.maxRowLength()), function (cellIndex, index) {
          return (
            <CSVDataTableCellView
              key={index}
              cell={self.props.row[cellIndex]}
              problem={self.props.model.problemWithCell(self.props.index, cellIndex)}
            />
          );
        })}

        {rowProblem &&
          <td>
            <div className="problem problem-row">{rowProblem.description()}</div>
          </td>
        }
      </tr>
    );
  }
});

module.exports = CSVDataTableRowView;
