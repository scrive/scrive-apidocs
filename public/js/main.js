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

// Modal
(function($) {
	
	$.fn.modal = function(o) {	
		var s = {
			persistent: false,
			source: '',
			width: 498
		};
		
		if(o) $.extend(s, o);
		
		var source = $(s.source);
		var html = {
			header: source.children('#head').html(),
			body: source.children('#body').html()
		}
		
		return this.each( function() {
			$(this).click( function(e) {
				e.preventDefault();
				
				createModal();
			});
		});
		
		function createModal() {
			var wrap = $('<div>');
			var container = $('<div>');
			
			wrap.addClass('modal-wrap').appendTo('body');
			container.addClass('modal-container').appendTo('body');
			
			// Call a bunch of functions to populate the modal
			setHeader(container);
			setBody(container);	
			setFooter(container);
			setDimension(container);
			
			$('.modal-container').fadeIn('fast');
			
			// Remove the modal when clicking the white background or the close button (if persistent == false)
			if(s.persistent == false) {
				$('.modal-wrap, .modal-header a').click( function() {
					$('.modal-container, .modal-wrap').fadeOut('fast', function() {
						$('.modal-wrap, .modal-container').remove();
					});
				});
			}
		}
		
		// Set content for the modal header
		function setHeader(c) {
			c.append('<div class="modal-header">' + 
					 '<div class="modal-icon ' + s.icon + '"></div>' + 
					 '<div class="modal-title">' + html.header + '</div>' + 
					 '<a href="#" class="modal-close no-txt">Stäng</a>' + 
					 '<div class="clearfix"></div>' + 
					 '</div>');
			
			c.append('<div class="modal-spacer"></div>');
		}
		
		// Set content for the modal body
		function setBody(c) {
			c.append('<div class="modal-content">' + html.body + '</div>');
		}
		
		// Set content for the modal footer
		function setFooter(c) {
			c.append('<div class="modal-footer">' + 
					 '<a href="#" id="cancel">Avbryt</a>' + 
					 '<a href="#" id="message">Skicka meddelande</a>' + 
					 '<a href="#" class="btn-small green float-right">' + 
					 '<div class="left"></div><div class="label">Skicka!<div class="btn-symbol arrow-right"></div></div><div class="right"></div>' + 
					 '</a></div>');
		}
		
		// Set dimensions and center the modal
		function setDimension(c) {
			c.css({
				'margin-left': -(s.width/2),
				'margin-top': -(c.height()/2),
			})
		}
	}
})(jQuery);

$(document).ready( function() {
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
	
	$('.tweet').tweet({
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
});