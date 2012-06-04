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

//used all over the place to do the text info on the text inputs
function enableInfoTextOnce(where) {
  if (!where) {
    where = $(document);
  }
  var selector = 'input[infotext] ,  textarea[infotext]';
  var inputs = $(selector, where);

  function setInfoText(obj) {
    var input = $(obj);
    if (input.val() == "") {
      if ($.browser.msie && $.browser.version >= 9) {
        input = input.not(":focus");
      } else {
        input = input.not($(document.activeElement));
      }
      input.addClass("grayed");
      input.val(input.attr("infotext"));
    }
  }

  var removeInfoText = function(input) {
    if (input.hasClass("grayed")) {
      input.val("");
      input.removeClass("grayed");
    }
  };

  inputs.live("focus", function() {
    var input = $(this);
    removeInfoText(input);
  });

  inputs.live("keydown", function() {
    var input = $(this);
    removeInfoText(input);
  });

  inputs.live("blur", function() {
    setInfoText(this);
  });

  inputs.each(function() {
    setInfoText(this);
  });

  $("form").live("submit", function() {
    var elems = $(this).find(".grayed");
    elems.val("");
    return true;
  });
}

safeReady(function() {
  showModal();
  enableInfoTextOnce();
});

//load pages is used when looking at attachment view
safeReady(function() {
  loadpages();
});

function loadpages() {
  console.log("loading pages . . .");
  if (typeof(window.documentid) != "undefined") {
    var myurl;
    if (typeof(window.siglinkid) != "undefined" && typeof(window.sigmagichash) != "undefined")
      myurl = "/pagesofdoc/" + documentid + "/" + siglinkid + "/" + sigmagichash;
    else
      myurl = "/pagesofdoc/" + documentid;
    $.ajax({
      url: myurl,
      success: function(data) {
        var content = $(data);
        var errormsg = content.find(".errormsg");
        if (errormsg.length > 0) {
          Confirmation.popup({
              submit: new Submit({url: '/d' }) ,
              title: localization.problemWithPDF,
              content: errormsg.text(),
              cantCancel: true,
              acceptColor: "red",
              acceptText: localization.backToArchive
          });
        } else {
          $('#documentBox').html(content);
          bgok = true;
          console.log("bgok: " + bgok);
          $('.pagejpg').each(checkbgimageok);
        }
      },
      error: repeatForeverWithDelay(1000)
    });
  }
}

var bgok = true;

function checkbgimageok(i, el) {
  console.log("checking bg image");
  var url = $(el).css('background-image').replace('url(', '').replace(')', '').replace(/'/g, '').replace(/"/g, '');
  console.log(url);
  var bgImg = $('<img />');
  bgImg.hide();
  bgImg.load(function() {
    console.log("finished loading bg for check");
    var complete = $(this)[0].complete;
    var width = $(this).width();
    console.log("width of bg image: " + width);

    if (bgok && (!complete || !width)) {
      bgok = false;
//       loadpages();
    }
    bgImg.remove();
  }).error(function() {
    console.log("here");
    bgImg.remove();
    if (bgok) {
      bgok = false;
      loadpages();
    }
  });
  console.log("appending");
  $('body').append(bgImg);
  bgImg.attr('src', url);
  console.log("finished");
}

//used by the tos you get when you enter as an initial
//system user
safeReady(function() {
  $("#toscontainer").overlay({
    mask: standardDialogMask,
    load: true,
    fixed: false
  });
});

//used by the frontpage to show the legal popup
safeReady(function() {
  $(".openmodal").overlay({
    mask: standardDialogMask,
    top: standardDialogTop,
    fixed: false
  });
});

//not really sure why we have this - but it seems like
//something that's a bug fix - em
safeReady(function() {
  $(window).resize();
});

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

//used by the login and forgot password modals
function showModal() {
  var modalbox = $(".modalbox");
  modalbox.overlay({
    mask: {
      color: standardDialogMask,
      top: standardDialogTop,
      loadSpeed: 0,
      opacity: 0.9
    },
    speed: 0,
    fixed: false,
    top: standardDialogTop
  });
  if (modalbox.size() > 0) {
    modalbox.first().data("overlay").load();
  }
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

standardDialogMask = "#333333";
standardDialogTop = "10%";

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
    var tokenTag = $('<input type="hidden" name="xtoken">');
    var token = readCookie("xtoken");
    if (token && token.length > 0) {
      console.log(token);
      tokenTag.attr("value", token);
      form.append(tokenTag);
    }
  });
});

// i haven't the foggiest what this is doing, it says it adds attachment images
// add attachment images
safeReady(function() {
  if (typeof(window.documentid) != "undefined") {
    var myurl;
    if (typeof(window.siglinkid) != "undefined" && typeof(window.sigmagichash) != "undefined")
      myurl = "/sv/" + documentid + "/" + siglinkid + "/" + sigmagichash;
    else
      myurl = "/dv/" + documentid;
    $.ajax({
      url: myurl,
      success: function(data) {
        var content = $(data);
        var errormsg = content.find(".errormsg");
        if (errormsg.length > 0) {
          Confirmation.popup({
            submit: new Submit({url: '/d' }) ,
            title: localization.problemWithPDF,
            content: errormsg.text(),
            cantCancel: true,
            acceptColor: "red",
            acceptText: localization.backToArchive
          });
        } else {
          $('#attachmentbox').html(content);
        }
      },
      error: repeatForeverWithDelay(1000)
    });
  }
});

//does some google analytics stuff, not sure if this is working or not
safeReady(function() {
  $("form.requestAccount").submit(function() {
    if (window._gaq != undefined) 
        _gaq.push(['_trackEvent', 'Signup', 'Click']);
  });
});

//shows the video on the front page
safeReady(function() {
  $(".campaign-play-video").click(function() {
    window.open('https://player.vimeo.com/video/41846881', '', 'scrollbars=no,menubar=no,height=500,width=700,resizable=yes,toolbar=no,location=no,status=no');
    return false;
  });
});

//when we moved to backbone we stopped using this function, is it still handy
//tho?!
/*
 * Function to deal with the situation when page is very big (more then 3000px)
 * So when basiclly all page, normally seen as 10 or more pages are interpreted
 * by the browser as one big page. This happends on iPod or in embedded frames,
 *
 * Then we want to put overlay near the button that activates it
 */
function saveOverlay(d, o) {
  $(d).click(function() {
    if ($(this).data("overlay") == undefined) {
      if ($(window).height() < 1650) {
        o.top = standardDialogTop;
      } else {
        o.top = $(this).offset().top - $(document).scrollTop() - 400;
      }
      o.load = true;
      o.fixed = false;
      $(this).overlay(o);
    }
    return false;
  });
}

//seems unecessary to clear the inputs, but i guess this is here because
//it fixed a bug
safeReady(function() {
  $(document).unload(function() {
    $("input").each(function() {
      var i = $(this);
      if (i.val().trim() === i.attr('infotext').trim()) {
        i.val("");
      }
    });
  });
});

//not sure what this does
safeReady(function() {
  var oldsignableform = $(".jsuploadform");
  var signableform;
  $(".jsuploadform").ajaxForm({
    success: function(d) {
      if (d)
        window.location.href = d.designurl;
    },
    error: function(a, b) {
      LoadingDialog.close();
      if (b === 'parsererror')
        FlashMessages.add({content: localization.fileTooLarge, color: "red"});
      else
        FlashMessages.add({content: localization.couldNotUpload, color: "red"});
      oldsignableform.replaceWith(signableform);
      oldsignableform = signableform;
      signableform = signableform.clone(true);
    },
    dataType: 'json'
  });
  // this is kind of a hack. need more backbone!
  signableform = oldsignableform.clone(true);
});

//Checking
$(document).ready(function() {
  if ($.browser.msie && $.browser.version < "7.0") {
    var alertModal = $("<div class='modal-container' style='height:80px'>" + "<div class='modal-body' style='padding:20px;font-size:13pt'>" + "<div class='modal-icon decline' style='margin-top:0px'></div>" + "<div>" + localization.ie6NotSupported + "</div>" + "" + "</div>");
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

  // needed for IE8 (awesomeness of this browser is unbelivable)
  if ($.browser.msie) {
    var loginForm = $("#loginForm");
    loginForm.keypress(function(event) {
      if (event.keyCode == 13)
        loginForm.submit();
    });
  }
});

// Alternative submit button
(function($) {
  $.fn.altSubmit = function() {
    return this.each(function() {
      $(this).click(function(e) {
        e.preventDefault();
        var name = $(this).attr('rel');
        var form = null;
        if ($(this).attr('form') != undefined)
            form = $($(this).attr('form'));
        else
            form = $(this).parents('form');
        form.append('<input type="hidden" name="' + name + '" value="1" />').submit();
      });
    });
  };
})(jQuery);

// tooltip
(function($) {
  $.fn.tooltip = function() {
    return this.each(function() {
      var container = $('<div>');
      $(this).mouseenter(function(e) {
        container.appendTo('body').addClass('tooltip-container').css({
          left: $(this).offset().left + $(this).width() + 19,
          top: $(this).offset().top - 20
        }).html('<div class="tooltip-arrow"></div><div class="tooltip-body">' + $($(this).attr('rel')).html() + '</div>');
      }).mouseleave(function() {
        container.remove();
      });
    });
  };
})(jQuery);

//stuff for public pages
$(document).ready(function() {
  //this stuff is for the login/forgot password popup
  var emailform = $(".login-container input[type=email]");
  emailform.focus();
  if (emailform.length > 0 && emailform.val().length > 0) {
      $(".login-container[input[type=password]").focus();
  }

  $('.tooltip').tooltip();

  $('.login-button').click(function(e) {
    var newlocation = window.location.pathname + window.location.search;
    if (newlocation.match(/[\?&]logging(&|=|$)/) === null) {
        if (window.location.search == "")
            newlocation += "?logging";
        else
            newlocation += "&logging";
    }
    window.location = newlocation;
    return false;
  });

  $('.recovery-container .txt-link').click(function(e) {
    e.preventDefault();

    $("#exposeMask").css("display", "none");
    $('.recovery-container').data("overlay").close();
    $("#exposeMask").css("display", "block");
    $('.login-container').data("overlay").load();
    return false;
  });
  $('.login-container .txt-link').click(function(e) {
    e.preventDefault();
    $("#exposeMask").css("display", "none");
    $('.login-container').data("overlay").close();
    $("#exposeMask").css("display", "block");
    $('.recovery-container').data("overlay").load();
    return false;
  });

  //this loads the tweets
  var tweetelems = $(".tweet");
  if (tweetelems.length > 0) {
    setTimeout(function() {
      tweetelems.tweet({
        username: localization.tweetAccount,
        count: 3,
        loading_text: localization.loadingTweets
      });
    }, 1000);
  }

  $('a.submit').altSubmit();
});
