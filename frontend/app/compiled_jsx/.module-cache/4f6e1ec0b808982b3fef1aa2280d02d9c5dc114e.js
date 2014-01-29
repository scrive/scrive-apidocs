define(function() {
  /**
   *  Convinient function to build url from url fragments
   *
   *  Usage:
   *     Without GET parameters:
   *        buildAbsoluteUrl('archive', 'downloadmainfile'); => /archive/downloadmainfile/
   *
   *     With GET parameters as dictionary in the end
   *          buildAbsoluteUrl('archive', {id:1}); => /archive/downloadmainfile.pdf/?id=1&
   *
   *   TODO: Add tests.
   *         Better comments.
   *         Make it possible to use url fragments like this:
   *              buildAbsoluteUrl('/api/', '/archive'). Right now it becomes '/api//archive'.
   */
  var buildAbsoluteUrl = function() {
    var parameters = null,
    argumentsArray = Array.apply(null, arguments), // arguments is an object, convert to Array
    indexLastElement = argumentsArray.length-1,
    lastElement = argumentsArray[indexLastElement];

    // check if last item in argumentsArray is an object
    if(lastElement !== null && typeof lastElement === 'object') {
      // remove object from argumentsArray, since we want to process it separatly
      parameters = lastElement;
      argumentsArray.pop(indexLastElement);
    }

    var url = '';
    // Add / in start of url, if there isnt one already in first url fragment
    if(argumentsArray[0].charAt(0) !== '/') {
      url += '/';
    }

    // parse url fragments
    _.each(argumentsArray, function(argument) {
      url += argument + '/';
    });


    // Add supplied dictionary as GET parameters at end of 'url' string
    if(parameters) {
      url += '?';
      _.each(parameters, function(value, key) {
        url += key + '=' + value + '&';
      });
    }
    return url;
  };

  /**
   *  javascript setInterval() but for limited amount of time.
   *
   *  @params {number} pollInterval Time between ticks
   *  @params {number} pollTimeout Stop the poller when it have been polling for x ms
   *  @params {function} callOnTick Function to call between every tick
   */
  var setLimitedInterval = function(pollInterval, pollTimeout, callOnTick) {
    var ticks = 0,
    totalRunTime = 0,
    stop;

    // Called every tick
    var poller = function() {

      ticks += 1;
      totalRunTime = ticks * pollInterval;

      // Only poll server, to see if document have been sealed for a limited amount of time.
      if(totalRunTime >= pollTimeout) {
        stop.cancel();
        $log.debug('Poller timeout reached.');
      } else {
	// dont call first round, when we set up timeout
	if(stop) {
	  // Call supplied code that should be run on every tick
	  callOnTick(stop);
	}

        // Keep polling
        stop = $timeout(poller, pollInterval);
	
	// Convinient function to cancel timeout from the place that started the poller
	// without importing $timeout and doing $timeout.cancel(stop)
	stop.cancel = function() {
	  $timeout.cancel(stop);
	};
	
      }
    };
    poller();

    return stop;
  };

  return {
    buildAbsoluteUrl: buildAbsoluteUrl,
    setLimitedInterval: setLimitedInterval
  };
});
