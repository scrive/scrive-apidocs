var React = require("react");
var Select = require("../../common/select");
var $ = require("jquery");
var _ = require("underscore");
var Language = require("../../../js/utils/language.js").Language;


module.exports = React.createClass({
  languages: function () {
    if (!this._languages) {
      var knownLanguages = Language.allLanguagesOptions();
      this._languages = _.sortBy(knownLanguages, function (l) {
        return l.name.toLowerCase();
      });
    }

    return this._languages;
  },
  render: function () {
    var self = this;
    return (
      <Select
        ref="select"
        isOptionSelected={function (o) {
          return o.value == self.props.model.lang();
        }}
        onSelect={function (v) {
          self.props.model.setLang(v);
          self.forceUpdate();
          return true;
        }}
        options={self.languages()}
        width={240}
      />
    );
  }
});
