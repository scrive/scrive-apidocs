define(function() {
  var expose = {};

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
   */
  expose.buildAbsoluteUrl = function() {
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
  
  return expose;
});
