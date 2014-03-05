/** @jsx React.DOM */
/* Sample code from: http://eldar.djafarov.com/2013/11/reactjs-mixing-with-backbone */

define(['Backbone', 'legacy_code'], function() {

var expose = {};

var BackboneMixin = {
  componentDidMount: function() {
    // Whenever there may be a change in the Backbone data, trigger a reconcile.
    this.getBackboneModels().forEach(this.injectModel, this);
  },
  componentWillUnmount: function() {
    // Ensure that we clean up any dangling references when the component is
    // destroyed.
    this.__syncedModels.forEach(function(model) {
      model.off(null, model.__updater, this);
    }, this);
  },
  injectModel: function(model){
    if(!this.__syncedModels) this.__syncedModels = [];
    if(!~this.__syncedModels.indexOf(model)){
      var updater = this.forceUpdate.bind(this, null);
      model.__updater = updater;
      model.on('add change remove', updater, this);
      this.__syncedModels.push(model);
    }
  }
};



expose.BackboneMixin = BackboneMixin;

return expose;


});
