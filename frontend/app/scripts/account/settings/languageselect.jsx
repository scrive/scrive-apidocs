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
    var lname = _.findWhere(self.languages(), {value: self.props.model.lang()}).name;
    var options = _.filter(self.languages(), function (l) {
      return l.value != self.props.model.lang() && !l.hidden;
    });

    return (
      <Select
        ref="select"
        name={lname}
        onSelect={function (v) {
          self.props.model.setLang(v);
          self.forceUpdate();
          return true;
        }}
        options={options}
        width={240}
        // This is a hack - since footer has a fixed height, and this select box is very big
        // it may cause problems. So we expand the footer to match its size.
        onOpen={function () {
          // right now the expanded select is still not mounted, so this value is pre-select-existience
          var documentHeight = $(document).height();
          var bodyPaddingHeight = $("footer").offset().top + $("footer").outerHeight();
          bodyPaddingHeight = bodyPaddingHeight - $(".body-container").offset().top - $(".body-container").height();
          setTimeout(function () {
            // and now the document (maybe) is bigger
            var newDocumentHeight = $(document).height();
            if (newDocumentHeight > documentHeight) {
            var heightDifference = newDocumentHeight - documentHeight;
            $(".body-container").css("padding-bottom", bodyPaddingHeight + heightDifference + "px");
            $("footer").css("height", $("footer").height() + heightDifference + "px");
            }
          }, 100);
          return true;
        }}
        onClose={function () {
          $(".body-container").css("padding-bottom", "");
          $("footer").css("height", "");
        }}
      />
    );
  }
});
