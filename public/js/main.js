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
	
	$('input.infotext, textarea.infotext').each( function() {
		var orig_value = $(this).val();
	
		$(this).focus( function() {
			if($(this).val() == orig_value) $(this).val('');
		}).blur( function() {
			if($(this).val() == '') $(this).val(orig_value);
		});
	});
	
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
	
	$('#sub-remove-btn').live('click', function() {
		var row = $(this).parents('tr');
		var i = 0;
		
		row.nextAll().each( function() {
			if($(this).is('.odd')) $(this).removeClass('odd');
			else $(this).addClass('odd');
		});
		
		row.siblings('.new').each( function() {
			$(this).find('input').each( function() {
				var oldAttr = $(this).attr('name').split('-');
				var newAttr = oldAttr[0] + '-' + (i+1);
			
				$(this).attr('name', newAttr);
			});
			
			i++;
		});

		row.remove();
	});
	
	$('#subaccount-add-sub').click( function() {
		var container = $('#selectable');
		var rowClass = (container.find('tr:last').is('.odd')) ? '' : 'odd';
		var i = container.children('.new').length+1;
		
		container.append('<tr class="' + rowClass + ' new">' +
		'<td><input type="checkbox" name="subcheck" class="check" /></td>' + 
		'<td><input type="text" name="firstname-' + i + '" value="Förnamn" class="infotext" /></td>' + 
		'<td><input type="text" name="lastname-' + i + '" value="Efternamn" class="infotext" /></td>' + 
		'<td><input type="text" name="position-' + i + '" value="Befattning" class="infotext" /></td>' + 
		'<td><input type="text" name="dept-' + i + '" value="Avdelning" class="infotext" /></td>' + 
		'<td><input type="text" name="phone-' + i + '" value="Telefonnummer" class="infotext" /></td>' + 
		'<td><input type="text" name="email-' + i + '" value="Email" class="infotext" /></td>' + 
		'<td><a href="javascript:;" class="icon small remove" id="sub-remove-btn"></a></td>' + 
		'</tr>');
		
		$('#selectable input.infotext').each( function() {
			var orig_value = $(this).val();

			$(this).focus( function() {
				if($(this).val() == orig_value) $(this).val('');
			}).blur( function() {
				if($(this).val() == '') $(this).val(orig_value);
			});
		});
	});
});
