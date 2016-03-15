// We always set timezone cookie, so it can be reused if we will get a sudden redirect to /newdocument
$(document).ready(function() {
    if (window.Cookies != undefined)
      Cookies.set("timezone",jstz.determine().name());
});
