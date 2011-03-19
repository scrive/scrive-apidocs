// Alternative submit button
(function($) {
	$.fn.altSubmit = function() {	
		return this.each( function() {
			$(this).click( function(e) {
				e.preventDefault();
				
				var name = $(this).attr('rel');
				
				$(this).parents('form').append('<input type="hidden" name="' + name + '" value="1" />').submit();
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

function initInfotexts(s)
{
        s.each(function() {
        var orig_value = $(this).val();
    
        $(this).focus( function() {
            if($(this).val() == orig_value) $(this).val('');
            $(this).removeClass("grayed");          
        }).blur( function() {
            if($(this).val() == '') {
                $(this).val(orig_value);
                $(this).addClass("grayed");          
            }
        });
    });
}

$(document).ready( function() {
	$('.tooltip').tooltip();
	
	$('.login-button').click( function(e) {
		e.preventDefault();
	
		$('.user-container').fadeToggle('fast');
                $('.login-container').find('.body input').first().focus();
	});
	
	$('.recovery-container .top, .login-container .top').click( function(e) {
		e.preventDefault();
	
		$('.user-container').fadeToggle('fast');
	});
	
	$('.login-container .txt-link, .recovery-container .txt-link').click( function(e) {
		e.preventDefault();
	
		$('.login-container, .recovery-container').fadeToggle('fast');
	});
	
	$('.user-container').each( function() {
		$(this).mouseleave( function() {
			if($(this).is(':visible')) {
				/*$.data(this, 'user', setTimeout( function() {
					$('.user-container').stop(true, true).fadeOut('fast');
				}, 1000));*/
			}
		}).mouseenter( function() {
			clearTimeout($.data(this, 'user'));
		});
	});

	setTimeout(function(){$('.tweet').tweet({
		username: 'skrivapa',
		count: 3,
		loading_text: 'Laddar tweets..'
	});},1000)
	
	initInfotexts($('input.infotext, textarea.infotext'));
	
	// Options dropdown on archive, sub accounts etc.
	$('.tab-dd').click( function() {
		$(this).children('.tab-dd-opts').toggle();
	}).mouseleave( function() {
		$.data(this, 'dd', setTimeout( function() {
			$('.tab-dd').children('.tab-dd-opts').hide();
		}, 500));
	}).mouseenter( function() {
		clearTimeout($.data(this, 'dd'));
	});
	
	$('a.submit').altSubmit();
	
	$('#subaccount-add-sub').click( function() {
		var container = $('#selectable');
		var rowClass = (container.find('tr:last').is('.odd')) ? '' : 'odd';
		if (container.children('.newSubaccount').empty()) {
		var newrow = $('<tr class="' + rowClass + ' newSubaccount">' +
		'<td><input type="checkbox" name="subcheck" class="check" /></td>' + 
		'<td><input type="text" name="fstname" infotext="Förnamn" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="sndname" infotext="Efternamn" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="companyposition" infotext="Befattning" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="phone" infotext="Telefonnummer" class="infotext grayed" /></td>' + 
		'<td><input type="text" name="email" infotext="Email" class="infotext grayed" /></td>' + 
        '<td>' + 
            '<a href="javascript:;" class="icon small add"></a>' + 
            '<a href="javascript:;" class="icon small remove"></a>' +
        '</td>' + 
		'</tr>');
        container.append(newrow);
        $(".add",newrow).click(function(){
            $(this).parents("form").append("<input type='hidden' name='add' value='YES'>").submit();
        })
        $(".remove",newrow).click(function(){
            newrow.remove();
        })
        }
		initInfotexts($('input.infotext',newrow));
	});
});
