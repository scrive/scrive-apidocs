//Checking 
$(document).ready( function() {
    if ($.browser.msie && $.browser.version < "7.0")
    {
       var alertModal = $("<div class='modal-container' style='height:80px'>"+
                        "<div class='modal-body' style='padding:20px;font-size:13pt'>"+
                        "<div class='modal-icon decline' style='margin-top:0px'></div>"+
                        "<div>Din webbläsare, Internet Explorer 6, stöds inte längre. Vänligen uppgradera till en modern webbläsare</div>"+
                        ""+
                        "</div>"
                         )
      $("body").html("");
      $("body").append(alertModal);
      alertModal.overlay({
        load: true,
        closeOnClick: false,
        closeOnEsc: false,
        mask: {
            color: '#000000',
            loadSpeed: 0,
            opacity: 0.90
            }
         });   
    } 
});


// Alternative submit button
(function($) {
    $.fn.altSubmit = function() {   
        return this.each( function() {
            $(this).click( function(e) {
                e.preventDefault();
                var name = $(this).attr('rel');
                var form = $(this).parents('form');
                if ($(this).attr('form') != undefined)
                    form = $($(this).attr('form')); 
                form.append('<input type="hidden" name="' + name + '" value="1" />').submit();
            });
        });
    }
})(jQuery);

// tooltip
(function($) {
	$.fn.tooltip = function() {
		return this.each( function() {
			var container = $('<div>');
			$(this).mouseenter( function(e) {
				container
				.appendTo('body')
				.addClass('tooltip-container')
				.css({
					left: $(this).offset().left+30,
					top: $(this).offset().top-20
				})
				.html('<div class="tooltip-arrow"></div><div class="tooltip-body">' + $($(this).attr('rel')).html() + '</div>');
			}).mouseleave( function() {
				container.remove();
			});
		});	
	}
})(jQuery);

$(document).ready( function() {
        $(".login-container input[type=email]").focus();

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
	
	$('.recovery-container .txt-link').click( function(e) {
		e.preventDefault();
        
        $("#exposeMask").css("display","none");
        $('.recovery-container').data("overlay").close();
        $("#exposeMask").css("display","block");
        $('.login-container').data("overlay").load();
		
    })
	$('.login-container .txt-link').click( function(e) {
		e.preventDefault();
        $("#exposeMask").css("display","none");
        $('.login-container').data("overlay").close();
        $("#exposeMask").css("display","block");
        $('.recovery-container').data("overlay").load();
	    })	
	setTimeout(function(){$('.tweet').tweet({
		username: 'skrivapa',
		count: 3,
		loading_text: 'Laddar tweets..'
	});},1000)
	
	// Options dropdown on archive, sub accounts etc.
	$('.tab-dd').click( function() {
		$(this).children('.tab-dd-opts').toggle();
                var button = $(this).find(".tab-dd-button");
                if (button.hasClass("tab-dd-exp")) {
                  button.removeClass("tab-dd-exp");
                } else {
                  button.addClass("tab-dd-exp");
                }
	}).mouseleave( function() {
		$.data(this, 'dd', setTimeout( function() {
			$('.tab-dd').children('.tab-dd-opts').hide();
                        $('.tab-dd').find(".tab-dd-button").removeClass("tab-dd-exp");
		}, 500));
	}).mouseenter( function() {
		clearTimeout($.data(this, 'dd'));
	});
	
	$('a.submit').altSubmit();
	
	$('.subaccount-add-sub').click( function() {
		var container = $('#selectable');
		if ($('.newSubaccount',container).size()==0) {
        var newrow = $('<tr class="odd newSubaccount">' +
		'<td><input type="checkbox" name="subcheck" class="check" /></td>' + 
		'<td><input type="text" name="fstname" infotext="Förnamn" value="Förnamn" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="sndname" infotext="Efternamn" value="Efternamn" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="companyposition" infotext="Befattning" value="Befattning" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="phone" infotext="Telefonnummer" value="Telefonnummer" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="email" infotext="Email" value="Email" class="infotext grayed" /></td>' + 
        '<td style="text-align:center">' + 
            '<a href="javascript:;" class="icon small ok add"></a>' + 
            '<a href="javascript:;" class="icon small minus remove"></a>' + 
        '</td>' + 
		'</tr>');
        $("tr",container).toggleClass('odd');
        container.prepend(newrow);
        $(".add",newrow).click(function(){
            $(this).parents("form").append("<input type='hidden' name='add' value='YES'>").submit();
        })
        $(".remove",newrow).click(function(){
            $('.newSubaccount',container).remove();
            $("tr",container).toggleClass('odd');
        })
        }
	});
});
