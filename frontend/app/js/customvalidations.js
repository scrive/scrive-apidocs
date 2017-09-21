var Backbone = require("backbone");
var _ = require("underscore");

var defs = require("./defs");

var CustomValidation = Backbone.Model.extend(
  {
    defaults: {
      pattern: "",
      tooltipMessage: "",
      validExample: ""
    },
    pattern: function () {
      return this.get("pattern");
    },
    setPattern: function (newPattern) {
      return this.set("pattern", newPattern);
    },
    tooltipMessage: function () {
      return this.get("tooltipMessage");
    },
    setTooltipMessage: function (newTooltipMessage) {
      return this.set("tooltipMessage", newTooltipMessage);
    },
    validExample: function () {
      return this.get("validExample");
    },
    setValidExample: function (newValidExample) {
      return this.set("validExample", newValidExample);
    },
    validate: function (value) {
      var result = null;

      if (this.pattern()) {
        try {
          var regex = new RegExp(this.pattern());
        } catch (err) {
          return result;
        }

        result = regex.test(value);
      }

      return result;
    }
  },
  {
    PATTERNS: {
      email: defs.PATTERN_EMAIL,
      phone: defs.PATTERN_PHONE,
      numbers: "^[0-9]+$"
    },
    predefinedPatternKey: function (pattern) {
      return _.findKey(CustomValidation.PATTERNS, function (predefinedPattern) {
        return pattern == predefinedPattern;
      });
    }
  }
);

module.exports.CustomValidation = CustomValidation;
