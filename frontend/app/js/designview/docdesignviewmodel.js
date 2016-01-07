/*
 * Model for DesignView
 */

define([ 'Backbone', 'legacy_code'], function(Backbone) {

  window.DesignViewModel = Backbone.Model.extend({
      initialize: function (args) {
          var self = this;
          var model = this;
      },
      document : function() {
          return this.get("document");
      },
      ready : function() {
          return this.document().ready();
      },
      participantDetail: function() {
          return this.get('participantDetail');
      },
      setParticipantDetail: function(s) {
          if (s == undefined) {
            this.trigger("visibility:participantclosed");
          } else {
            this.trigger("visibility:participantopen");
          }
          this.set({participantDetail : s});
          return this;
      },
      saveDocument: function() {
          var viewmodel = this;
          var document = this.document();

          var wasSaved = document.saved();
          document.setSaved();
          document.save(function () {
            viewmodel.saveFlashMessage(wasSaved);
          });
      },
      saveAndFlashMessageIfAlreadySaved: function() {
          var self = this;
          var isSaved = self.document().saved();
          self.document().save(function () {
            if(isSaved) {
              self.saveFlashMessage(true);
            }
          });
      },
      saveFlashMessage: function(wasSaved) {
          var flashMsg;
          if(this.document().isTemplate()) {
            if(wasSaved) {
              flashMsg = localization.designview.saved.saveTemplate;
            } else {
              flashMsg = localization.designview.saved.savedAsTemplate;
            }
          }
          else {
            if(wasSaved) {
              flashMsg = localization.designview.saved.saveDraft;
            } else {
              flashMsg = localization.designview.saved.savedAsDraft;
            }
          }
          new FlashMessage({type: 'success', content: flashMsg});
      }
  });
});
