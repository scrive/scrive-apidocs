var _ = require("underscore");

var FlashMessage = require("../../js/flashmessages.js").FlashMessage;

module.exports = {
  _getDocument: function () {
    if (_.isFunction(this.getDocument)) {
      return this.getDocument();
    }

    return this.props.document;
  },
  saveDocument: function () {
    var wasSaved = this._getDocument().saved();
    this._getDocument().setSaved();

    var self = this;
    this._getDocument().save(function () {
      self.saveFlashMessage(wasSaved);
    });
  },
  saveAndFlashMessageIfAlreadySaved: function () {
    var isSaved = this._getDocument().saved();

    var self = this;
    this._getDocument().save(function () {
      if (isSaved) {
        self.saveFlashMessage(true);
      }
    });
  },
  saveFlashMessage: function (wasSaved) {
    var flashMsg = null;
    if (this._getDocument().isTemplate()) {
      if (wasSaved) {
        flashMsg = localization.designview.saved.saveTemplate;
      } else {
        flashMsg = localization.designview.saved.savedAsTemplate;
      }
    } else {
      if (wasSaved) {
        flashMsg = localization.designview.saved.saveDraft;
      } else {
        flashMsg = localization.designview.saved.savedAsDraft;
      }
    }

    new FlashMessage({type: "success", content: flashMsg});
  }
};
