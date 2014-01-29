define(['Underscore'], function(_) {
  var expose = {};

  /**
   *  @description
   *  Re-render component when backbone model updates 
   * 
   *  @note
   *  Taken from http://eldar.djafarov.com/2013/11/reactjs-mixing-with-backbone/
   */
  expose.ModelForceUpdate = {
    __syncedModels: [],

    /**
     *  @description
     *  Whenever there may be a change in the Backbone data, trigger a reconcile.
     *
     *  @note
     *  If we have a componentDidMount in the class that use this Mixin, the componentDidMount
     *  of that function need to be definied before including the mixin. Else that functions
     *  componentDidMount wont be called.
     *
     *  Should be possible to solve with some super()?
     */
    componentDidMount: function() {
      this.getBackboneModels().forEach(this.injectModel, this);
    },

    /**
     *  @description
     *  Ensure that we clean up any dangling references when the component is
     *  destroyed.
     */
    componentWillUnmount: function() {
      this.__syncedModels.forEach(function(model) {
	model.off(null, model.__updater, this);
      }, this);
    },

    injectModel: function(model){
      if(!~this.__syncedModels.indexOf(model)){
	var updater = this.forceUpdate.bind(this, null);
	model.__updater = updater;
	model.on('add change remove', updater, this);
      }
    }
  };

  /**
   *  @description
   *  2-way data-binding for Backbone Models
   *
   *  @note
   *  Taken from http://eldar.djafarov.com/2013/11/reactjs-mixing-with-backbone/
   */
  expose.BindMixin = {
    bindTo: function(model, key){
      return {
	value: model.get(key),
	requestChange: function(value){
          model.set(key, value);
	}.bind(this)
      }
    }
  };

  return expose;
});
