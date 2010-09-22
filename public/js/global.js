

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
    // FIXME: try to extract css property margin-top and margin-bottom from #mainContainer
    $("#mainContainer").height( $(this).height() - $('#headerContainer').height() - $('#footerContainer').height() - 25 - 25 );
}

function signatoryadd()
{
    var signatorylist = $( "#signatorylist" );
    var sig = $("#signatory_template").clone();
	var text = sig.html();
	var emailfield = sig.find(".emailvalidation");
	emailfield.attr("id","othersignatoryemail");
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

function swedishString(names)
{ 
    if( names.length == 0) 
        return "";
    if( names.length == 1) 
        return "<strong>" + names[0] + "</strong>";
  
    var name0 = names.shift();  
    if( names.length == 1)
        return "<strong>" + name0 + "</strong> och " + swedishString(names);

    return "<strong>" + name0 + "</strong>, " + swedishString(names);
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

    //$("#dialog-confirm-signinvite").hide();
    //$("#dialog-confirm-sign").hide();

    $("#signinvite").click(function() {
         var button = $(this);
         var mrxs = $("form input[name='signatoryname']");
         var tot = "";
         if(!emailFieldsValidation($('input.emailvalidation'))){
             return false;
         }
         else{
             var allparties = new Array();
             mrxs.each(function(index) { 
                     allparties.push($(this).val());
                 });
             var tot = swedishString(allparties);
             $(".Xinvited").html(tot);
             $("#dialog-confirm-signinvite").dialog({
                     resizable: false,
                         // autoOpen: false,
                         height: 340,
                         width: 350,
                         modal: true,
                         buttons: {
                         'Underteckna': function() {
                             var form = $("#form");
                             var name = button.attr("name");
                             form.append("<input type='hidden' name='" + name + "' value='automatic'>");
                             //alert(tot);
                             form.submit();
                         },
                             'Avbryt': function() {
                                 $(this).dialog('close');
                             }
                     }
                 });
         }
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
	
	$("input.emailvalidation").focus(function(){
		applyRedBorder($(this));
		return false;
    });
	
	$("#loginbtn").click(function(){
		if(emailFieldsValidation($('input.emailvalidation'))){
			$("form").submit();
		}						  
		return false;
	});
	
	$("#createnewaccount").click(function(){
		if(emailFieldsValidation($('input.emailvalidation'))){
			$("form").submit();
		}
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

$("#othersignatoryemail").live('focus', function(e){	
		    applyRedBorder($(this));
});

	
function isValidEmailAddress(emailAddress) {
	var pattern = /^([A-Za-z0-9_\-\.])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,4})$/;
	return pattern.test(emailAddress);
}

function applyRedBorder(field){
	field.keyup(function(){
		var emailVal = $(this).val();
		$(this).removeAttr("style");		
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
}
	   

  function emailFieldsValidation(fields){
      var invalidEmailErrMsg = "Felaktig e-post \"email\". Försök igen.";
      var emptyEmailErrMsg = "Du måste ange e-post till motpart.";
	 var errorMsg="";
	 var address="";
	 var showError=false;
	 var isValidEmail=false;

    
    fields.each(function() {
		if(!isExceptionalField($(this))){
		address = $(this).val();
		 
			if(address.length == 0){
                            errorMsg = emptyEmailErrMsg;
                            showError = true;
			}
			if(isValidEmailAddress(address) == false && showError==false) { 
				errorMsg=invalidEmailErrMsg.replace("email",address);
				showError=true;
			}
			 
			if(showError){
				var $dialog = $('<div></div>')
					.html(errorMsg)
					.dialog({
						autoOpen: false,
						title: 'Felaktig e-post',
						modal: true
					});
				$dialog.dialog('open');
				return false;
			}	
		}
	});
	isValidEmail=(showError)?false:true;
	return isValidEmail;
}

function isExceptionalField(field){

	var parentid = field.closest("div").attr("id");
	var fieldid = field.attr("id");
	var fieldname=field.attr("name");

	if(fieldname=="signatoryemail" && parentid == "signatory_template" && fieldid != "othersignatoryemail")
		return true

	return false
}

function editinvitetext()
{
    var txt = $("#invitetext").val();
    $('#edit-invite-text-dialog').dialog({
                     height: 280,
                     width: 350,
                     modal: true,
                     buttons: {
                     'OK': function() {
                         var newtxt = $("#edit-invite-text-dialog textarea").val();
                         $("#invitetext").val( newtxt );
                             $(this).dialog('close');
                     },
                         'Avbryt': function() {
                             $(this).dialog('close');
                         }
                 }
             });
}