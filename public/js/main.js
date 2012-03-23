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

$(document).ready(function() {
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

    // Options dropdown on archive, sub accounts etc.
    $('.list-dd').click(function() {
        $(this).children('.list-dd-opts').toggle();
        var button = $(this).find(".list-dd-button");
        if (button.hasClass("list-dd-exp")) {
            button.removeClass("list-dd-exp");
        } else {
            button.addClass("list-dd-exp");
        }
        return false;
    }).mouseleave(function() {
        $.data(this, 'dd', setTimeout(function() {
            $('.list-dd').children('.list-dd-opts').hide();
            $('.list-dd').find(".list-dd-button").removeClass("list-dd-exp");
        }, 500));
    }).mouseenter(function() {
        clearTimeout($.data(this, 'dd'));
    });

    $('a.submit').altSubmit();
});

