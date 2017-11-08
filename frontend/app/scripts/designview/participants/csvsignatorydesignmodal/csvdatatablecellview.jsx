var React = require("react");

var CSVProblemModel = require("./csvproblemmodel");

var CSVDataTableCellView = React.createClass({
  propTypes: {
    cell: React.PropTypes.string.isRequired,
    problem: React.PropTypes.instanceOf(CSVProblemModel)
  },
  render: function () {
    return (
      <td>
        <div className="content">{this.props.cell}</div>
        {this.props.problem &&
          <div className="problem">{this.props.problem.description()}</div>
        }
      </td>
    );
  }
});

module.exports = CSVDataTableCellView;
