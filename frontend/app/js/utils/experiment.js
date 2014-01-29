/* Wrapper around session storage to set up a/b/c tests and persist their values across the session */

define(['Backbone', 'legacy_code'], function() {

/* Create an experiment named 'name' in namespace 'namespace' 
 * where the possible values are in domain. Domain has to be an array of strings.
 *
 * @param args An argument object, with the attributes described above.
 *
 * @return Object with a getter function, 'value'.
 *
 * Example:
 * var buttonExperiment = new Experiment({
 *   name: 'buttonColor', 
 *   namespace: 'button', 
 *   domain: ['green', 'blue', 'red']
 * });
 *
 * if (buttonExperiment.value() == 'green') { ... }
 *
 * To generate a new experiment.
 *
 * Example:
 * var buttonExperiment = new Experiment({namespace: 'button', name: 'buttonColor'});
 * 
 * if (buttonExperiment.value() == 'red') { ... }
 *
 * to get the value of the current button experiment.
 */
window.Experiment = function(args) {
  this.value = function() {
    return SessionStorage.get(args.namespace, args.name);
  };

  // No domain means we don't generate the experiment, we just readonly it.
  if (!args.domain) {
    return this;
  }

  var index = Math.floor(Math.random()*args.domain.length);
  var value = args.domain[index];

  // only strings due to SessionStorage 
  if (! typeof value == "string") console.log("WARNING: adding non-string to experiment. domain should only be strings");
  
  SessionStorage.set(args.namespace, args.name, value);

};

});
