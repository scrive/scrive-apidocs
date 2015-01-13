/** @jsx React.DOM */

define(['Backbone', 'legacy_code'], function() {

var expose = {};

var BackboneMixin = {

  // We want to be able to listenTo events on React component
  componentWillMount : function() {_.extend(this, Backbone.Events);},
  // Whenever there may be a change in the Backbone data, trigger a reconcile.
  componentDidMount: function() {this.updateBackboneModelsBinding();},
  // The backbone models might depend on state or props, and we need to re-inject if the models have changed.
  componentDidUpdate: function() {this.updateBackboneModelsBinding();},
  // When component gets unmounted, we should stop listening to events
  componentWillUnmount: function() {this.dropBackboneModelsBinding();},


  // For all backbone models, start listening on new models and stop on old ones
  updateBackboneModelsBinding: function() {
    var self = this;

    if(!self.__syncedModels) self.__syncedModels = [];

    // Prepare a list of models to be droped, to be injected and to be left
    var newModels = self.getBackboneModels();
    var removedSynchedModels = _.difference(self.__syncedModels,newModels);
    var remainingSynchedModels = _.difference(self.__syncedModels,removedSynchedModels);
    // Unbind models that are no longer backbone model
    removedSynchedModels.forEach(function(model) {
      self.stopListening(model);
    });

    // Internal state should be set to models that are left
    self.__syncedModels = remainingSynchedModels;

    // New backbone models should be injected
    _.difference(newModels,remainingSynchedModels).forEach(self.injectModel, self);
  },
  injectModel: function(model){
    var self = this;
    if(!self.__syncedModels) self.__syncedModels = [];
    if(!~self.__syncedModels.indexOf(model)){
      self.__syncedModels.push(model);
      var updater = self.forceUpdate.bind(this, null);
      self.listenTo(model,'add change remove', function() {
        if (~self.__syncedModels.indexOf(model)) {
          updater();
        }
      });
    }
  },
  dropBackboneModelsBinding: function() {
    var self = this;
    self.__syncedModels.forEach(function(model) {
      self.stopListening(model);
    });
    self.__syncedModels = [];
  }
};

expose.BackboneMixin = BackboneMixin;

return expose;


});
