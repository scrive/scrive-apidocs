define([], function() {
  var expose = {};

  /**
   * @name Common.truncate
   * @description Truncate string Filter
   *
   * @param {string} text - Input string to truncate
   * @param {number} length - Default is 10
   * @param {string} end - default is "..."
   * @return {string} - Truncated string
   *
   * @example
   * var myText = "This is an example.";
   *
   * truncate('mytext')
   * truncate('mytext', 5)
   * truncate('mytext', 25, '->')
   * Output
   * "This is..."
   * "Th..."
   * "This is an e ->"
   *
   */
  expose.truncate = function (text, length, end) {
      // Don't try to truncate empty strings.
      // empty strings may be sent in to this filter when the data that
      // we try to interpolate havent came back from api
      if(typeof text === 'undefined' || text === null || text === '') {
        return;
      }
      
      if (isNaN(length)) {
        length = 10;
      }
      
      if (end === undefined) {
        end = '...';
      }
      
      if (text.length <= length || text.length - end.length <= length) {
        return text;
      }
      else {
        return String(text).substring(0, length-end.length) + end;
      }
      
  };

  return expose;
});
