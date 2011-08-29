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

/*
 * For the decline button on the account activation after signing dialog
 */
$(document).ready(function() {
    $(".declineAccountFromSign").click(function() {
        var form = $(this).closest("form");
        var acceptaccount = form.find("input[name='acceptaccount']");
        var declineaccount = $("<input type='hidden' name='declineaccount' value='true' />");
        declineaccount.insertBefore(acceptaccount);
        acceptaccount.remove();
        form.submit();
    });
});

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
    });

    $('.recovery-container .txt-link').click(function(e) {
        e.preventDefault();

        $("#exposeMask").css("display", "none");
        $('.recovery-container').data("overlay").close();
        $("#exposeMask").css("display", "block");
        $('.login-container').data("overlay").load();
    });
    $('.login-container .txt-link').click(function(e) {
        e.preventDefault();
        $("#exposeMask").css("display", "none");
        $('.login-container').data("overlay").close();
        $("#exposeMask").css("display", "block");
        $('.recovery-container').data("overlay").load();
    });
    var tweetelems = $(".tweet");
    if (tweetelems.length > 0) {
        setTimeout(function() {
            tweetelems.tweet({
                username: 'skrivapa',
                count: 3,
                loading_text: localization.loadingTweets
            });
        }, 1000);
    }

    // Options dropdown on archive, sub accounts etc.
    $('.tab-dd').click(function() {
        $(this).children('.tab-dd-opts').toggle();
        var button = $(this).find(".tab-dd-button");
        if (button.hasClass("tab-dd-exp")) {
            button.removeClass("tab-dd-exp");
        } else {
            button.addClass("tab-dd-exp");
        }
    }).mouseleave(function() {
        $.data(this, 'dd', setTimeout(function() {
            $('.tab-dd').children('.tab-dd-opts').hide();
            $('.tab-dd').find(".tab-dd-button").removeClass("tab-dd-exp");
        }, 500));
    }).mouseenter(function() {
        clearTimeout($.data(this, 'dd'));
    });

    $('a.submit').altSubmit();

    $('.subaccount-add-sub').click(function() {
        var container = $('#selectable');
        if ($('.newSubaccount', container).size() == 0) {
            var newrow = $('<tr class="odd newSubaccount">' + '<td><input type="checkbox" name="subcheck" class="check" /></td>' + '<td><input type="text" name="fstname" infotext="' + localization.fstname + '" value="' + localization.fstname + '" class="infotext grayed" /></td>' + '<td><input type="text" name="sndname" infotext="' + localization.sndname + '" value="' + localization.sndname + '" class="infotext grayed" /></td>' + '<td><input type="text" name="companyposition" infotext="' + localization.companyposition + '" value="' + localization.companyposition + '" class="infotext grayed" /></td>' + '<td><input type="text" name="phone" infotext="' + localization.phone + '" value="' + localization.phone + '" class="infotext grayed" /></td>' + '<td><input type="email" name="email" infotext="' + localization.email + '" value="' + localization.email + '" class="infotext grayed" /></td>' + '<td style="text-align:center">' + '<a href="javascript:;" class="icon small ok add"></a>' + '<a href="javascript:;" class="icon small minus remove"></a>' + '</td>' + '</tr>');
            $("tr", container).toggleClass('odd');
            container.prepend(newrow);
            $(".add", newrow).click(function() {
                var form = $(this).parents("form");
                var emailfield = form.find("input[name='email']");
                var email = emailfield.val();

                var userdomain = useremail.replace(/.*@/, "");
                var emaildomain = email.replace(/.*@/, "");

                /** these two lines are a very temporary hack to satisfy a customer that must start using multiple domains tomorrow
                    sorry about this - em, a proper fix is planned (SKRIVAPADEV-578) but it's gonna involve db changes, so leaving until db
                    migration is done **/
                var isSpecialCaseDomain = function(domain) {
                  return (domain == "resursbemanning.se") || (domain == "itresurs.se") || (domain == "ekonomresurs.se");
                };
                var isprodhack = isSpecialCaseDomain(userdomain) && isSpecialCaseDomain(emaildomain);

                if (!isprodhack && emaildomain != userdomain) {
                    FlashMessages.add({content: localization.youCanNotInviteSameEmail , color:  "red"});
                } else {
                    form.append("<input type='hidden' name='add' value='YES'>").submit();
                }
            });
                $(".remove", newrow).click(function() {
                $('.newSubaccount', container).remove();
                $("tr", container).toggleClass('odd');
            });
        }
    });
});

function repeatSlide() {
    doSlide('.slide-content');
}
                        
function doSlide(elm) {
    var items = jQuery(elm).length;
    var current = jQuery(elm + '.active').index()-1;
    
    if(current == (items-1)) {
        jQuery(elm).eq(current).fadeOut().removeClass('active');
        jQuery(elm).eq(0).fadeIn().addClass('active');
    } else {
        jQuery(elm).eq(current).fadeOut('slow').removeClass('active');
        jQuery(elm).eq(current+1).fadeIn('slow').addClass('active');
    }
    
    jQuery('.slide-nav ul li').removeClass('active').eq(jQuery(elm + '.active').index()-1).addClass('active');
    
    jQuery.data(this, 'slide', setTimeout( function() {
        repeatSlide();
    }, 10000));
}

function gotoPage(elm, index) {
    var items = jQuery(elm).length;
    
    if(index < items) {
        clearTimeout(jQuery.data(this, 'slide'));
        
        jQuery(elm).filter('.active').fadeOut().removeClass('active');
        jQuery('.slide-nav ul li').removeClass('active');
        
        jQuery(elm).eq(index).fadeIn().addClass('active');
        jQuery('.slide-nav ul li').eq(index).addClass('active');
        
        jQuery.data(this, 'slide', setTimeout( function() {
            repeatSlide();
        }, 10000));
    }
}

jQuery(document).ready( function() {
    jQuery('.campaign-text a, .campaign-play').overlay({mask: standardDialogMask, fixed:false});
    
    jQuery('.slide-nav ul li').click( function() {
        var index = jQuery(this).index();
        
        gotoPage('.slide-content', index);
    });
    
    doSlide('.slide-content');
});
