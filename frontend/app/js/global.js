// add startsWith to String prototype
if (typeof String.prototype.startsWith != 'function') {
  String.prototype.startsWith = function (str){
    return this.slice(0, str.length) == str;
  };
}

require(['Backbone', 'legacy_code'], function(Backbone) {

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

window.trackTimeout = function(name, props, trackTimeoutCallBack, ms) {
    var called = false;
    ms = ms || 300;
    mixpanel.track(name, props, function(e) {
        if(called)
            return;
        called = true;
        if( trackTimeoutCallBack ) {
            return trackTimeoutCallBack(e);
        }
    });
    setTimeout(function(e) {
        if(called)
            return;
        called = true;
        if( trackTimeoutCallBack ) {
            return trackTimeoutCallBack(e);
        }
    }, ms);
};

});

//make sure we've got console logging/warning
if (!window.console) {
  window.console = {};
}

if (!window.console.log) {
  window.console.log = function() {};
}

if (!window.console.warn) {
  window.console.warn = function(s) {
    mixpanel.track('window.console.warn() stub: ' + s);
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

function parseQueryString() {
    var match,
        urlParams = {},
        pl     = /\+/g,  // Regex for replacing addition symbol with a space
        search = /([^&=]+)=?([^&]*)/g;
    var decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); };
    var query  = window.location.search.substring(1);

    while (match = search.exec(query))
       urlParams[decode(match[1])] = decode(match[2]);
    return urlParams;
}

require(['Backbone', 'legacy_code'], function() {

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
    if ($("input[name='xtoken']",form).size() == 0 && $(form).attr('method') != undefined && $(form).attr('method').toUpperCase() == 'POST') {
      var tokenTag = $('<input type="hidden" name="xtoken">');
      var token = Cookies.get("xtoken");
      if (token && token.length > 0) {
        console.log(token);
        tokenTag.attr("value", token);
        form.append(tokenTag);
      }
    }
  });
});

// We always set timezone cookie, so it can be reused if we will get a sudden redirect to /newdocument
$(document).ready(function() {
    if (window.Cookies != undefined)
      Cookies.set("timezone",jstz.determine().name());
});


});

function capitaliseFirstLetter(string)
{   if (string == undefined || string.length == 0) return string;
    return string.charAt(0).toUpperCase() + string.slice(1);
}

// Utility that allows changing of names into nicely localized span on <strong> tags, separated with ',' and localized 'and'
// This function will escape texts from the list
// Example: buildBoldList(['a','b','c']) -> <strong>a</strong>, <strong>b</strong> and <strong>c</strong>
function buildBoldList(list) {
    var strongsString = "";
    for(var i=0;i<list.length;i++) {
      if (list.length - i > 2) {
        strongsString += "<strong/>, ";
      } else if (list.length - i == 2) {
        strongsString += "<strong/> " + localization.listand + " ";
      } else {
        strongsString += "<strong/>";
      }
    }
    var res = $("<span/>").html(strongsString);
    var strongs = res.find("strong");
    for(var i=0;i<list.length;i++) {
      $(strongs[i]).text(list[i]);
    }
    return res;
}

