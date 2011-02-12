function openModal(html, o) {
			
	var defaults = {
		height: '380',
		width: '480',
		topTxt: ''
	};
	
	if(o) $.extend(defaults, o);
	
	$('<div>')
	.addClass('modal-wrap')
	.appendTo('body');
	
	$('<div>')
	.addClass('modal-container')
	.css({
		'height': o.height,
		'width': o.width,
		'margin-left': -(o.width/2),
		'margin-top': -(o.height/2)
	})
	.append('<div class="modal-header">SkrivaPå<a href="#" class="btn-tiny blue center">Stäng</a></div><div class="modal-top">' + o.topTxt + '</div><div class="modal-body">' + html + '</div><div class="modal-footer"></div></div>')
	.appendTo('body');
	
	$('.modal-body').css('height', (o.height - 184));
	
	$('.modal-container').fadeIn('fast');
	
	$('.modal-wrap, .modal-header a').click( function() {
		$('.modal-container, .modal-wrap').fadeOut('fast', function() {
			$('.modal-wrap, .modal-container').remove();
		});
	});
	
	return true;
}

$(document).ready( function() {
	$('.login-button').click( function(e) {
		e.preventDefault();
	
		$('.user-container').fadeToggle('fast');
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
	
	$('.tweet').tweet({
		username: 'skrivapa',
		count: 3,
		loading_text: 'Laddar tweets..'
	});
	
	$('input.infotext, textarea.infotext').each( function() {
		var orig_value = $(this).val();
	
		$(this).focus( function() {
			if($(this).val() == orig_value) $(this).val('');
		}).blur( function() {
			if($(this).val() == '') $(this).val(orig_value);
		});
	});
	
	$('form a.submit').click( function(e) {
		e.preventDefault();
		
		$(this).parents('form').submit();
	});
});