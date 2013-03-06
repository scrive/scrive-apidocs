// globally track errors
window.onerror = function(msg, url, line) {
    mixpanel.track('Error', {
        Message         : 'Javascript Error',
        URL             : url,
        Line            : line,
        'Error Message' : msg
    });

    return false;
};

$(document).ajaxError(function(event, jqxhr, settings, exception) {
    mixpanel.track('Error', {
        Message         : 'Ajax Error',
        URL             : settings.url,
        Method          : settings.type,
        'Error Message' : exception.toString()
    });
});

window.trackTimeout = function(name, props, cb, ms) {
    var called = false;
    ms = ms || 300;
    mixpanel.track(name, props, function(e) {
        if(called)
            return;
        called = true;
        return cb(e);
    });
    setTimeout(function(e) {
        if(called)
            return;
        called = true;
        return cb(e);
    }, ms);
};

window.createnewdocument = function(event) {
  event.preventDefault();
  event.stopImmediatePropagation();
  trackTimeout('Click start new process', {}, function() {
      new Submit({
          method : "POST",
          url : "/api/frontend/createfromfile",
          ajax: true,
          expectedType : "text",
          ajaxsuccess: function(d) {
              try {
                  window.location.href = "/d/"+JSON.parse(d).id;
              } catch(e) {
                  LoadingDialog.close();
              }
          }
      }).send();
  });
}

window.createfromtemplate = function(event) {
    event.preventDefault();
    event.stopImmediatePropagation();
    trackTimeout('Click create from template', {}, function() {
        window.location.href = "/fromtemplate";
    });
    return false;
}

//make sure we've got console logging
if (!window.console) {
  window.console = {
    log: function() {}
  };
}

//ie doesn't support trim naturally! maybe we should use underscore trim
if (typeof String.prototype.trim !== 'function') {
  String.prototype.trim = function() {
    return this.replace(/^\s+|\s+$/g, '');
  };
}

//used by global.js, signup.js and bankid.js
function safeReady(f) {
  $(function() {
    try {
      f();
    } catch (e) {
      console.log(e);
    }
  });
}

//used by global.js and bankid.js
function repeatForeverWithDelay(delay) {
  return function() {
    var that = this;
    $(document).delay(delay).queue(function() {
      $(this).dequeue();
      $.ajax(that);
    });
  };
}

//used by buttons for signing/sending/saving/rejecting document
function alreadyClicked(button) {
  var result = false;
  if (button.wasAlreadyClicked)
    result = true;
  button.wasAlreadyClicked = true;
  return result;
}

//used by the administration pages
function prepareForEdit(form, width) {
  var width = width == undefined ? 540 : width;

  $(".editable", form).each(function() {
    var textarea = $("<textarea style='width:" + width + "px;height:0px;border:0px;padding:0px;margin:0px'  name='" + $(this).attr('name') + "'> " + $(this).html() + "</textarea>");
    var wrapper = $("<div></div>").css("min-height", ($(this).height()) + 15 + "px");
    wrapper.append(textarea);
    $(this).replaceWith(wrapper);
    var editor = prepareEditor(textarea);
  });
  $(".replacebynextonedit", form).each(function() {
    var replacement = $(this).next();
    $(this).replaceWith(replacement);
    replacement.show();
  });
}

function prepareEditor(textarea) {
  return textarea.tinymce({
    script_url: '/tiny_mce/tiny_mce.js',
    theme: "advanced",
    theme_advanced_toolbar_location: "top",
    theme_advanced_buttons1: "bold,italic,underline,separator,strikethrough,bullist,numlist,separator,undo,redo,separator,cut,copy,paste",
    theme_advanced_buttons2: "",
    convert_urls: false,
    theme_advanced_toolbar_align: "left",
    plugins: "noneditable,paste",
    valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul"
  });
}

function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for (var i = 0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(nameEQ) == 0) {
      return c.substring(nameEQ.length);
    }
  }
  return null;
}

function parseQueryString() {
    var match,
        urlParams = {},
        pl     = /\+/g,  // Regex for replacing addition symbol with a space
        search = /([^&=]+)=?([^&]*)/g,
        decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
        query  = window.location.search.substring(1);

    while (match = search.exec(query))
       urlParams[decode(match[1])] = decode(match[2]);
    return urlParams;
}

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
safeReady(function() {
  $("form").live('submit', function() {
    var form = $(this);
    if ($("input[name='xtoken']",form).size() == 0 && $(form).attr('method').toUpperCase() == 'POST') {
      var tokenTag = $('<input type="hidden" name="xtoken">');
      var token = readCookie("xtoken");
      if (token && token.length > 0) {
        console.log(token);
        tokenTag.attr("value", token);
        form.append(tokenTag);
      }
    }
  });
});

//Checking
$(document).ready(function() {
    if ($.browser.msie) {
        var ver = parseInt($.browser.version, 10);
        if( ver < 7 && ver > 0 ) {
            /*
             * If we could not make sense of User agent string from
             * Explorer, then just let the person sign. No need to be
             * too picky on people that are spohisticated.
             */
            mixpanel.track('Old IE popup', {'Browser version' : ver});
            var alertModal = $("<div class='modal-container' style='height:80px'>" +
                                 "<div class='modal-body' style='padding:20px;font-size:13pt'>" +
                                   "<div class='modal-icon decline' style='margin-top:0px'></div>" +
                                   "<div>" + localization.ie6NotSupported + "</div>" +
                                 "</div>" +
                               "</div>");
            $("body").html("");
            $("body").append(alertModal);
            alertModal.overlay({
                load: true,
                closeOnClick: false,
                closeOnEsc: false,
                fixed: false,
                mask: {
                    color: '#000000',
                    loadSpeed: 0,
                    opacity: 0.90
                }
            });
        }
    }
});

function capitaliseFirstLetter(string)
{
    return string.charAt(0).toUpperCase() + string.slice(1);
}
