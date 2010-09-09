

function getUniqueId()
{
    var rnd = Math.round(Math.random() * 1000000000);
    while($("#" + rnd).length  >0) {
        rnd = Math.round(Math.random() * 1000000000);
    }
    return rnd;
}

function enableInfoText(where)
{
    if( where==null ) {
        where = $(document);
    }
    var inputs = where.find('input[type="text"][infotext!=""]');
    
    inputs.focus(function() { 
            if( $(this).hasClass("grayed")) {
                $(this).val("");
                $(this).removeClass("grayed");
            }
            });
    inputs.blur(function() {
            if( $(this).val()=="" || $(this).val()==$(this).attr("infotext") ) {
                $(this).addClass("grayed");
                $(this).val($(this).attr("infotext"));
            }
        });
    inputs.blur();
    $("form").submit(function () {
            // this is so wrong on so many different levels!
            $('input[type="text"][infotext!=""].grayed').val("");
        });
}

function disableInfoText(where)
{
    if( where==null ) {
        where = $(document);
    }
    inputs = where.filter('input[type="text"][infotext!=""]');
    inputs.focus();
}

function resizeToWindow()
{
    $("#mainContainer").height( $(this).height() - $('#headerContainer').height() - $('#footerContainer').height() - 2 );
}

function signatoryadd()
{
    var signatorylist = $( "#signatorylist" );
    var sig = $("#signatory_template").clone();
    signatorylist.append(sig);
    enableInfoText(sig);
    sig.hide();
    sig.slideDown("slow");
    return false;
}

function signatoryremove(node)
{
  var sig = $(node).parent();
  var cls = sig.data("draggableBoxClass");
  var db = $("." + cls);
  sig.slideUp('slow',function() { $(this).remove(); });
  db.fadeOut('slow',function() { $(this).remove(); });
  return false;
}


$(document).ready( function () {
    $('.flashmsgbox').delay(5000).fadeOut();
    $('.flashmsgbox').click( function() { 
         $(this).fadeOut() 
    });
    $('#all').click(function() {
            var c = $('input:checkbox[name="doccheck"]');
            var acc = true;
            c.each(function(i, val) { acc = acc && $(val).attr("checked");});
            c.attr("checked", !acc);
    });
    enableInfoText();
    if(typeof(window.documentid)!= "undefined" ) {
        $.ajax({ url: "/pagesofdoc/" + documentid,
            success: function(data) {
                $('#documentBox').html(data);
            },
            error: function () {
                var that = this;
                $(document).delay(1000).queue(function() {
                        $(this).dequeue();
                        $.ajax(that);
                    });
            }
        });
    }
    $("#dialog-confirm-signinvite").hide();
    $("#dialog-confirm-sign").hide();
    $("#signinvite").click(function() {
         var button = $(this);
         var mrxs = $("form input[name='signatoryname']");
         var tot = "";
         mrxs.each(function(index) {
                 if( tot!="" ) tot += ", ";
                 tot += $(this).val();
             });
         $("#mrx").text(tot);

         $("#dialog-confirm-signinvite").dialog({
                 resizable: false,
                     height: 340,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Underteckna': function() {
                         var form = $("#form");
                         var name = button.attr("name");
                         form.append("<input type='hidden' name='" + name + "' value='automatic'>");
                         form.submit();
                     },
                         'Avbryt': function() {
                             $(this).dialog('close');
                         }
                 }
             });

         return false;});

    $("#sign").click(function() {
         var button = $(this);
         $("#dialog-confirm-sign").dialog({
                 resizable: false,
                     height: 280,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Underteckna': function() {
                         var form = $("#form");
                         var name = button.attr("name");
                         form.append("<input type='hidden' name='" + name + "' value='automatic'>");
                         form.submit();
                     },
                         'Avbryt': function() {
                             $(this).dialog('close');
                         }
                 }
             });
         
         return false;
    });
    $(window).resize(resizeToWindow);
    $(window).resize();
    
    //var rpxJsHost = (("https:" == document.location.protocol) ? "https://" : "http://static.");
    //document.write(unescape("%3Cscript src='" + rpxJsHost +
    //           "rpxnow.com/js/lib/rpx.js' type='text/javascript'%3E%3C/script%3E"));
    //    RPXNOW.overlay = true;
    //    RPXNOW.language_preference = 'sv';
});

// Email Validation on submit
$(document).ready(function(){
  $("form").submit(function(e){
     var invalidEmailErrMsg="Value in the \"Email\" field was not recognized. Please make sure to put valid email address";
	 var emptyEmailErrMsg="Please specifiy email address:";
	 var errorMsg="";
     var address=$("input[name*='email']").val();
	 var showError=false;
	 
	 
	 if(address.length == 0){
	   errorMsg=emptyEmailErrMsg
	   showError=true;
	 }
	 if(isValidEmailAddress(address) == false && showError==false) { 
		errorMsg=invalidEmailErrMsg;
        showError=true;
     }
	 
	 if(showError){
	  //$("#errorMsg").html(errorMsg);
	   var $dialog = $('<div></div>')
			.html(errorMsg)
			.dialog({
				autoOpen: false,
				title: 'Error',
				modal: true
			});
		$dialog.dialog('open');
	  return false;
	 }
  });
});

//Email Validation on key up
$(document).ready(function() {
       $("input[name*='email']").focus(function(){
		    $(this).keyup(function(){
		   // alert(email.css());
				var emailVal = $(this).val();
			
				if(emailVal != 0)
				{
					if(isValidEmailAddress(emailVal))
					{
						$(this).removeAttr("style");
					}
					else{
					  $(this).css("border","1px solid red");
					} 
				}
				else{
					 $(this).removeAttr("style");
				} 
				
				});
		});
	
	});
	
function isValidEmailAddress(emailAddress) {
	var pattern = /^([A-Za-z0-9_\-\.])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,4})$/;
	return pattern.test(emailAddress);
}