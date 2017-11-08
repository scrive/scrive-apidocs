var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var GeneralProblemListView = React.createClass({
  propTypes: {
    problems: React.PropTypes.array.isRequired
  },
  render: function () {
    return (
      <div className="generalProblems">
        {_.map(this.props.problems, function (problem, index) {
          var className = classNames("problem", {
            "problem-header": problem.header()
          });

          return (
            <div key={index} className={className}>
              {problem.description()}
            </div>
          );
        })}
      </div>
    );
  }
});

module.exports = GeneralProblemListView;
