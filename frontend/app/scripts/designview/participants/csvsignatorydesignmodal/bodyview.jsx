var React = require("react");
var _ = require("underscore");

var CSVSignatoryDesignModel = require("./csvsignatorydesignmodel");
var CSVDataTableRowView = require("./csvdatatablerowview");
var GeneralProblemListView = require("./generalproblemlistview");
var utils = require("./utils");

var BodyView = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(CSVSignatoryDesignModel).isRequired
  },
  render: function () {
    var self = this;

    return (
      <div className="designCSVSignatoryPopupScroll">
        <GeneralProblemListView
          problems={this.props.model.generalProblems()}
        />

        {this.props.model.header() &&
          <table className="csvDataTable">
            <thead>
              <tr>
                {_.map(this.props.model.header(), function (header, index) {
                  return (
                    <th key={index}>{utils.headerName(header)}</th>
                  );
                })}
              </tr>
            </thead>

            <tbody>
              {_.map(this.props.model.rows(), function (row, index) {
                return (
                  <CSVDataTableRowView
                    key={index}
                    index={index}
                    model={self.props.model}
                    row={row}
                  />
                );
              })}
            </tbody>
          </table>
        }
      </div>
    );
  }
});

module.exports = BodyView;
