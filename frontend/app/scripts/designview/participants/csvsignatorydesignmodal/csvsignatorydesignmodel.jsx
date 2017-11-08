var Backbone = require("backbone");
var _ = require("underscore");

var CSVProblemModel = require("./csvproblemmodel");
var Submit = require("../../../../js/submits.js");

var CSVSignatoryDesignModel = Backbone.Model.extend({
  csvstandardheaders: {
    "fstname": {type: "name", order: 1},
    "sndname": {type: "name", order: 2},
    "email": {type: "email"},
    "mobile": {type: "mobile"},
    "sigco": {type: "company"},
    "sigpersnr": {type: "personal_number"},
    "sigcompnr": {type: "company_number"}
  },
  defaults: {
    header: undefined,
    problems: [],
    rows: undefined
  },
  initialize: function () {
    this.onUpload = this.onUpload.bind(this);
  },
  problems: function () {
    return this.get("problems");
  },
  ready: function () {
    return (
      (this.rows() != undefined) && (this.rows().length > 0) &&
      (this.problems().length  == 0)
    );
  },
  header: function () {
    return this.get("header");
  },
  rows: function () {
    return this.get("rows");
  },
  csv: function () {
    return [this.header()].concat(this.rows());
  },
  maxRowLength: function () {
    var max = 0;
    _.each(this.rows(), function (row) {
      max = (max > row.length) ? max : row.length;
    });

    return max;
  },
  isEmpty: function () {
    return (
      ((this.header() == undefined || this.rows() ==  undefined)) &&
      (this.problems().length  == 0)
    );
  },
  hasData: function () {
    return (
      (this.header() != undefined && this.header().length > 0) ||
      (this.rows() != undefined && this.rows().length > 0)
    );
  },
  problemWithRow: function (row) {
    var result = undefined;
    _.each(this.problems(), function (problem) {
      if (problem.aboutRow(row)) {
        result = problem;
      }
    });

    return result;
  },
  problemWithCell: function (row, cell) {
    var result = undefined;
    _.each(this.problems(), function (problem) {
      if (problem.aboutCell(row, cell)) {
        result = problem;
      }
    });

    return result;
  },
  generalProblems: function () {
    var result = [];
    _.each(this.problems(), function (problem) {
      if (problem.generalProblem()) {
        result.push(problem);
      }
    });

    return result;
  },
  onUpload: function (response) {
    var problems = [];

    if (response.parseError) {
      problems.push(
        new CSVProblemModel({
          description: localization.csv.formatError,
          header: true
        })
      );
    } else {
      if (response.header == undefined || response.header.length < 3) {
        problems.push(
          new CSVProblemModel({
            description: localization.csv.atLeast3Columns
          })
        );
      }

      if (response.rows.length == 0) {
        problems.push(
          new CSVProblemModel({
            description: localization.csv.atLeast1Party
          })
        );
      }

      if (response.header != undefined) {
        for (var i = 0; i < response.header.length; i++) {
          response.header[i] = (
            _.keys(this.csvstandardheaders)[i] || response.header[i]
          );
        }
      }
    }

    this.set({
      "header": response.header,
      "rows": response.rows,
      "problems": problems
    });

    this.trigger("change");
  },
  upload: function (input) {
    var submit = new Submit.Submit({url: "/parsecsv", method: "POST"});
    submit.addInputs(input);
    submit.sendAjax(this.onUpload);
  }
});

module.exports = CSVSignatoryDesignModel;
