/**
 * For Cross-Site Request Forgery (CSRF) Attacks
 * 1. Grab the cookie in Javascript, which protects
 *    against cross-domain attacks.
 * 2. Cram it into a form before it submits.
 * 3. This must be checked on the server.
 *
 * NOTE: This only protects againsts form submits. Links
 *   do not get protected by this code.
 */
$(function() {
  $(document).on('submit', "form", function() {
    var form = $(this);
    if ($("input[name='xtoken']",form).size() == 0 && $(form).attr('method') != undefined && $(form).attr('method').toUpperCase() == 'POST') {
      var tokenTag = $('<input type="hidden" name="xtoken">');
      var tokens = Cookies.getMulti("xtoken");
      if (tokens && tokens[0].length > 0) {
        var token = tokens.join(";");
        console.log(token);
        tokenTag.attr("value", token);
        form.append(tokenTag);
      }
    }
  });
});
