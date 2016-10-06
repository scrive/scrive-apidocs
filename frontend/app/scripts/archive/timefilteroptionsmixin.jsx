var capitaliseFirstLetter = require("../common/capitalise_first_letter");

module.exports = {
  componentWillMount: function () {
    this._fromToFilterOptions = [];

    var time = new Date();
    var year = this.props.year;
    var month = this.props.month;
    while (year < time.getFullYear() || (year == time.getFullYear() && month <= time.getMonth() + 1)) {
      var name = capitaliseFirstLetter(localization.months[month-1].slice(0,3) + " " + year);
      this._fromToFilterOptions.push({
        name: name,
        fromValue: [{
          "filter_by": "mtime",
          "start_time": new Date(year, month - 1).toISOString()
        }],
        toValue: [{
          "filter_by": "mtime",
          "end_time": new Date(year, month).toISOString()
        }]
      });

      month++;
      if (month == 13) {
        month = 1;
        year++;
      }
    }
  }
};