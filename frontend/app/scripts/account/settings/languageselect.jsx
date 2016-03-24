var React = require("react");
var Select = require("../../common/select");
var $ = require("jquery");
var _ = require("underscore");

module.exports = React.createClass({
  languages: function () {
    if (!this._languages) {
      var known_languages = [
        {name: localization.languages.enInEn, value: "en"},
        {name: localization.languages.svInSv, value: "sv"},
        {name: localization.languages.deInDe, value: "de"},
        {name: localization.languages.frInFr, value: "fr"},
        {name: localization.languages.itInIt, value: "it"},
        {name: localization.languages.esInEs, value: "es"},
        {name: localization.languages.ptInPt, value: "pt"},
        {name: localization.languages.nlInNl, value: "nl"},
        {name: localization.languages.daInDa, value: "da"},
        {name: localization.languages.noInNo, value: "no"},
        {name: localization.languages.elInEl, value: "el"},
        {name: localization.languages.fiInFi, value: "fi"},
        {name: localization.languages.isInIs, value: "is"},
        {name: localization.languages.etInEt, value: "et"},
        {name: localization.languages.lvInLv, value: "lv"},
        {name: localization.languages.ltInLt, value: "lt"},
      ];

      this._languages = _.sortBy(known_languages, function (l) {
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
