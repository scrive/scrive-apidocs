var Backbone = require("backbone");

var CSVProblemModel = Backbone.Model.extend({
  defaults: {
    row: undefined,
    cell: undefined,
    header: false,
    description: "Some problem"
  },
  row: function () {
    return this.get("row");
  },
  cell: function () {
    return this.get("cell");
  },
  header: function () {
    return this.get("header");
  },
  aboutCell: function (row, cell) {
    return this.row() == row && this.cell() == cell;
  },
  aboutRow: function (row) {
    return this.row() == row && this.cell() == undefined;
  },
  description: function () {
    return this.get("description");
  },
  generalProblem: function () {
    return this.row() == undefined && this.cell() == undefined;
  }
});

module.exports = CSVProblemModel;
