var CustomPatternMixin = {
  getCustomPatternName: function (patternKey) {
    switch (patternKey) {
      case "email":
        return localization.designview.customValidation.patternName.email;

      case "numbers":
        return localization.designview.customValidation.patternName.numbers;

      case "phone":
        return localization.designview.customValidation.patternName.phone;

      default:
        return "UNKNOWN_PATTERN";
    }
  }
};

module.exports = CustomPatternMixin;
