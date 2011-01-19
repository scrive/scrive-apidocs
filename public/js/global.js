
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
    var selector = ':input[infotext]';
    var inputs = where.find(selector);
    
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
            var elems = $(this).find(selector + ".grayed");
            elems.val("");
            return true;
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

function signatoryadd()
{
    var signatorylist = $( "#signatorylist" );
    var sig = $("#signatory_template").clone();
    sig.removeAttr("id");
	var emailfield = sig.find("input[type='email']");
	emailfield.addClass("othersignatoryemail"); 
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

        var focused;
        // two alternative ways to track clicks off of a .check
        //$("*").not(".check").click(function(){if(this == document.activeElement) { focused = null; }});
        // this way is cleaner since it only installs a handler on the toplevel document instead of nearly
        // every element
        $(document).click(function(event){ 
                if($(event.target).parents("#selectable").size() === 0){ 
                    focused = null; 
                }
            });

        $("#selectable tr").mousedown(function(event){
                if(focused && event.shiftKey && $(event.target).filter(".check").size() === 0){
                    var checks = $(".check");
                    var startIndex = focused?checks.index(focused):null;
                    var endIndex = checks.index($(this).find(".check"));
                    
                    var s = Math.min(startIndex, endIndex);
                    var e = Math.max(startIndex, endIndex);
 
                    checks.slice(s, e+1).attr("checked", true);

                    checks.not(":checked").parents("tr").removeClass("ui-selected");
                    checks.filter(":checked").parents("tr").addClass("ui-selected");

                    focused = $(checks.get(endIndex));
                    checks.get(endIndex).focus();

                    // cancel all further click processing
                    // we are overriding the Selectable behavior for shift-clicks
                    return false;
                }                     
            });

        // the jQuery Selectable feature 
        $("#selectable" ).selectable({
                // links and input fields do not have click overridden
                cancel: 'a,input',
                    
                unselected: function(event, ui) {
                    var check = $(ui.unselected).find(".check");
                    check.attr("checked", false);
                },

                selected: function(event, ui) {
                    var check = $(ui.selected).find(".check");
                    check.attr("checked", true);
                    check.focus();
                    focused = check;
                }});

        $(".check:checked").parents("tr").addClass("ui-selected");
        $(".ui-selected").find(".check").attr("checked", true);
        
        $(".check").click(function(event) {
                    
                if(event.shiftKey && focused && focused.filter(".check").size() > 0 && focused.attr("checked")){
                    var checks = $(".check");
                    var startIndex = checks.index(focused);
                    var endIndex = checks.index(this);
                    
                    var s = Math.min(startIndex, endIndex);
                    var e = Math.max(startIndex, endIndex);
 
                    var checksslice = checks.slice(s, e+1);
                    checksslice.attr("checked", true);
                    checksslice.parents("tr").addClass("ui-selected");
                }
                else {
                    if( $(this).attr("checked")) {
                        $(this).parents("tr").addClass("ui-selected");
                    }
                    else {
                        $(this).parents("tr").removeClass("ui-selected");
                    }
                }

                focused = $(this);
            });

        $('#all').click(function() {
            var checks = $('input:checkbox[name="doccheck"]');
            var acc = true;
            checks.each(function(i, val) { acc = acc && $(val).attr("checked");});
            checks.attr("checked", !acc);
            
            if( !acc ) {    
                checks.parents("tr").addClass("ui-selected");
            } else {
                checks.parents("tr").removeClass("ui-selected");
            }
        }); 

    flashFlashMessages();
    /*
    $('#all').click(function() {
    });
    */
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

    $("#addsiglink").click(function(){
	    $("#addsiglink").removeClass("redborder");
	    $("#authorroledropdown").removeClass("redborder");
	    
	});
    
 
    $("#signinvite").overlay({  
    mask: standardDialogMask,    
    onBeforeLoad: function () { 
           if (!emailFieldsValidation($("input[type='email']"))) return false;
           if (!authorFieldsValidation()) return false;
	   if (!nonZeroSignatories()) return false;
           var mrxs = $("form input[name='signatoryname']");
           var tot = "";
           var allparties = new Array();
             mrxs.each(function(index) { 
                     allparties.push($(this).val());
                 });
           tot = swedishString(allparties);
           $(".Xinvited").html(tot);
	    } });
        
    $(".submiter").click(function(){
                               $(this.form).submit();
	});

	$("#dialog-confirm-sign-eleg .bankid").click(function(){
		sign2(window.location.pathname,
		      "#dialog-confirm-sign-eleg",
		      "/bankid" + window.location.pathname);
		return false;
	    });
 
   $(".editer").each(function() {
                             $(this).click(function(){
                                  prepareForEdit($(this.form));
                                  $(this).hide();
                                  return false;
                                  })
				 })   ;
                         
    $("#editinvitetextlink").overlay({        
    mask: standardDialogMask,    
    onBeforeLoad: function () { 
            var newtxt = $("#invitetext").val()
            $("#edit-invite-text-dialog textarea").val(newtxt);    
            var author = $(".authorname").text();
	    var sigs = $(".signatorybox").not("#signatory_template");
            var partners = new Array()
            var i = 0;
            var authorSignes = jQuery("[name='authorrole']").val()!="secretary"
            var signedList =  jQuery(".authornamewhennotsecretary");                                                     
            if (authorSignes)
            {  
               partners[0] = author;
               i++;
               signedList.html(signedList.attr("okprefix")+" <strong>"+author+"</strong>");
            }
            else {
               signedList.html(signedList.attr("alt"));
            }
            sigs.each(function(){
              var namefield = $("[name='signatoryname']",this);
              var mailfield =  $("[name='signatoryemail']",this)
              
              var res = $(this).attr("alt");
              if (!namefield.hasClass("grayed"))          
                 res = namefield.val();
             else if (!mailfield.hasClass("grayed") )
                 res = mailfield.val(); 
              partners[i] = res
              i++;
            })
            $(".partylistupdate",$(this).attr("rel")).html(swedishList(partners));
    }
    })   
        
    $("#editing-invite-text-finished").click(function() {
                         var newtxt = $("#edit-invite-text-dialog textarea").val();
                         $("#invitetext").val( newtxt );
                     })
                         
    $(".redirectsubmitform").submit(function(){
                          var newform = $($(this).attr("rel"))
                          var inputs = $("input",$(this))
                          $('textarea:tinymce',$(this)).each(
                             function(){
                             inputs = inputs.add($("<input name='"+$(this).attr('name')+"' value='"+$(this).html()+"'>"))
                             })
                          inputs.css("display","none");
                          newform.append(inputs); 
                          newform.submit();
                          return false; 
                          })                      
    $("#sign").overlay({ mask: standardDialogMask,
        onBeforeLoad: function () { if (!sigFieldsValidation()) return false;}
    })

	$("#signbankid").overlay({ mask: standardDialogMask
		    });
    
    $("#cancel, .cancel").overlay({	mask: standardDialogMask
                });
    $("#toscontainer").overlay({mask: standardDialogMask,
                load: true
                });
	
    $("#loginbtn").click(function(){
		if(emailFieldsValidation($(":email",this.form))){
			$(this.form).submit();
		}						  
		return false;
	});

    $("#createnewaccount").click(function(){
		if(emailFieldsValidation($("input[type='email']"))){
			$(this.form).submit();
		}
		return false;
							
	
	});
    
    $(".validateEmail").click(function(){
                return (emailFieldsValidation($(":email",this.form)))
        });
    
    $(".flashOnClick").click( function(){
      $(".flashMessage",$(this).parent()).each(function(){
         addFlashMessage($(this).html())
         $(this).remove();
      })
    })    
    $(".addremovecheckbox").change(function(){
        var what = $($(this).attr("rel"))
        var location = $($(this).attr("location"))
        var oldlocation = $($(this).attr("oldlocation")) 
        if ($(this).val()=="off")
         {  
            location.append(what);      
            $(this).val("on"); 
            $(this).attr("checked","checked");
         } 
        else 
        {
            oldlocation.append(what);
            $(this).val("off");
            $(this).removeAttr("checked");
        }    
        return true;
        
    }).each(function(){
        if ($(this).val()=="on")
         {
            $($(this).attr("location")).append($($(this).attr("rel")));      
            $(this).attr("checked","checked");
         } })    
    
    $(".datetodaystip").each(function() {
         var curr = $(this);
         var basictime =  new Date().getTime();
         var daysinput = $(curr.attr("rel"));
         var localignore = true;
         curr.dateinput({
                 format: 'dd-mm-yy',
                     change: function() {
                     if (localignore) return false;
                     var ONE_DAY = 1000 * 60 * 60 * 24;
                     var date_ms = curr.data("dateinput").getValue().getTime();
                     var difference_ms = Math.abs(date_ms - basictime);            
                     var dist = Math.floor(difference_ms/ONE_DAY) + 1;
                     daysinput.val(dist);
                     curr.text("("+curr.data("dateinput").getValue('dd-mm-yy')+")"); 
                 },
                     min:  new Date()
                     });
        curr.data("dateinput").setValue(new Date());
        localignore = false;
        curr.data("dateinput").addDay(parseInt($(curr.attr("rel")).val()));
         
        daysinput.change(function(){
                 localignore = true
                 curr.data("dateinput").setValue(new Date());
                 localignore = false;
                 curr.data("dateinput").addDay(parseInt(daysinput.val()));
            });
        });
    $(".datetodate").each(function() {     
         var curr = $(this);
         var basictime =  new Date().getTime();
         var input = $(curr.attr("rel"));
         curr.dateinput({
             format:'dd-mm-yyyy',
             change: function() {
                 input.val(curr.data("dateinput").getValue('dd-mm-yyyy'));
                 curr.text(curr.data("dateinput").getValue('dd-mm-yyyy')); 
                 },
                     min:  new Date()
                     });
         if (input.val()=="") 
             curr.data("dateinput").setValue(new Date());
         else
             {  
                 date = input.val().split('-');
                 curr.data("dateinput").setValue(date[0],date[1],date[2]); 
             }
        });
    $(".replacebynextonclick").click( function(){
            var replacement = $(this).next();
            $(this).replaceWith(replacement);
            replacement.show();
        
        });
    
	if($("#authorroledropdown option:selected").val() == "signatory") {
		    $("#signinvite").val("Underteckna");
		    $(".buttonbox .submiter").text("Underteckna");

		    $("#signinvite").addClass("cross-button");

		    $("#dialog-title-sign").removeClass("hidden");
		    $("#dialog-title-send").addClass("hidden");

		    $("#dialog-confirm-text-sign").removeClass("hidden");
		    $("#dialog-confirm-text-send").addClass("hidden");

	} else if($("#authorroledropdown option:selected").val() == "secretary"){
		    $("#signinvite").val("Skicka");
		    $(".buttonbox .submiter").text("Skicka");

		    $("#signinvite").removeClass("cross-button");

		    $("#dialog-title-sign").addClass("hidden");
		    $("#dialog-title-send").removeClass("hidden");

		    $("#dialog-confirm-text-sign").addClass("hidden");
		    $("#dialog-confirm-text-send").removeClass("hidden");
		}

	$("#authorroledropdown").change(function() { 
		$("#addsiglink").removeClass("redborder");
		$("#authorroledropdown").removeClass("redborder");
		if($("#authorroledropdown option:selected").val() == "signatory") {
		    $("#signinvite").val("Underteckna");
		    $(".buttonbox .submiter").text("Underteckna");

		    $("#signinvite").addClass("cross-button");

		    $("#dialog-title-sign").removeClass("hidden");
		    $("#dialog-title-send").addClass("hidden");

		    $("#dialog-confirm-text-sign").removeClass("hidden");
		    $("#dialog-confirm-text-send").addClass("hidden");

		} else if($("#authorroledropdown option:selected").val() == "secretary"){
		    $("#signinvite").val("Skicka");
		    $(".buttonbox .submiter").text("Skicka");

		    $("#signinvite").removeClass("cross-button");

		    $("#dialog-title-sign").addClass("hidden");
		    $("#dialog-title-send").removeClass("hidden");

		    $("#dialog-confirm-text-sign").addClass("hidden");
		    $("#dialog-confirm-text-send").removeClass("hidden");

		}
	    });

    function gettext(id) {
	return $("#" + id).text();
    }
    
    $("#main-document-form").submit(function () {
	    var form = $("#main-document-form");
	    if($("#authorroledropdown option:selected").val() === "signatory") {
		form.append("<input type='hidden' name='signatoryname' value='"+ gettext('sauthorname') + "' />");
		form.append("<input type='hidden' name='signatorycompany' value='"+ gettext('sauthorcompany') + "' />");
		form.append("<input type='hidden' name='signatorynumber' value='"+ gettext('sauthornumber') + "' />");
		form.append("<input type='hidden' name='signatoryemail' value='"+ gettext('sauthoremail') + "' />");
	    }
	});
    
    $(window).resize();
    
    //var rpxJsHost = (("https:" == document.location.protocol) ? "https://" : "http://static.");
    //document.write(unescape("%3Cscript src='" + rpxJsHost +
    //           "rpxnow.com/js/lib/rpx.js' type='text/javascript'%3E%3C/script%3E"));
    //    RPXNOW.overlay = true;
    //    RPXNOW.language_preference = 'sv';
});

  function emailFieldsValidation(fields){
      fields = fields.filter(function(){return !isExceptionalField($(this))});
      if(fields.length > 0){
	  var inputs = fields.validator({
		  effect:'failWithFlashOnEmail',
		  formEvent: 'null'
	      });
	  var valid = inputs.data("validator").checkValidity();
	  return valid;
      }
      return true;
}

function authorFieldsValidation(){

    var remainingAuthFields = false;
    var remainingDragFields = false;
    $(".dragfield").each(function(){
	    var field = $(this);
	    var s = getFillStatus(field);
	    var ds = getDragStatus(field);
	    if(s == 'author') {
		remainingAuthFields = true;
	    }
	    if(ds == 'must place'){
		remainingDragFields = true;
	    }
	});
    var emptyMsg = "Please fill out all of the required fields.";
    var dragMsg = "Please drag all of the custom fields onto the document.";
    if(remainingAuthFields){
	var $dialog = $('<div></div>')
	    .html(emptyMsg)
	    .dialog({
		    autoOpen: false,
		    title: 'Required fields',
		    modal: true
		});
	$dialog.dialog('open');
	return false;
    } else if(remainingDragFields){
	var $dialog = $('<div></div>')
	    .html(dragMsg)
	    .dialog({
		    autoOpen: false,
		    title: 'Required fields',
		    modal: true
		});
	$dialog.dialog('open');
	return false;
    }
    return true;
}

function sigFieldsValidation(){

    var remainingSigFields = false;
	 
    $(".dragfield").each(function(){
	    var field = $(this);
	    if(getValue(field).length === 0) {
		remainingSigFields = true;
	    }
	});
    var emptyMsg = "Please fill out all of the fields.";
    if(remainingSigFields){
	var $dialog = $('<div></div>')
	    .html(emptyMsg)
	    .dialog({
		    autoOpen: false,
		    title: 'Required fields',
		    modal: true
		});
	$dialog.dialog('open');
	return false;
    }	
    return !remainingSigFields;
}

function nonZeroSignatories() {
    var sigs = 0;
    if($("#authorroledropdown option:selected").val() === "signatory"){
	sigs = 1;
    }

    sigs += $("#signatorylist .signatorybox").length;
    var error = (sigs === 0);

    if(error) {
	addFlashMessage('Det finns inga undertecknande parter för detta dokument. Vänligen lägg till undertecknande parter eller ändra din roll till "undertecknare".');
	$("#addsiglink").addClass("redborder");
	$("#authorroledropdown").addClass("redborder");
	return false;
    }
    return true;
}

function isExceptionalField(field){

	return (field.closest("div").attr("id") == "signatory_template")
}


$(function(){
    $(".prepareToSendReminderMail").each(function(){
        var form = $($(this).attr("rel"));
        $(this).overlay({
                     mask: standardDialogMask,
                     resizable: false,
                     onClose: function(e){ return false;
                    }
                 })
    })                   
})

function prepareForEdit(form){
    $(".editable",form).each( function(){
        
        var textarea = $("<textarea style='width:95%;height:0px;border:0px;padding:0px;margin:0px'  name='"+$(this).attr('name')+"'> "+ $(this).html()+ "</textarea>")
        var wrapper = $("<div></div>").css("min-height",($(this).height())+15+"px");
        wrapper.append(textarea);
        $(this).replaceWith(wrapper);
        var editor = prepareEditor(textarea)
            
  }) 
    $(".replacebynextonedit",form).each( function(){
        var replacement = $(this).next();
        $(this).replaceWith(replacement);
        replacement.show();
        
    })
}

function flashFlashMessages(){
    var flashmsgbox = $('.flashmsgbox');
    if ($('.flashmessage',flashmsgbox).size()>0)
     {
      flashmsgbox.show();
      flashmsgbox.delay(12000).fadeOut(function(){$('.flashmessage',flashmsgbox).remove()});
      flashmsgbox.click( function() { $(this).fadeOut()  });
     } 
}
function addFlashMessage(msg){
    var flashmsgbox = $('.flashmsgbox');    
    $('.flashmessage',flashmsgbox).remove()
    flashmsgbox.append("<span class='flashmessage' >"+msg+"</span>");
    flashFlashMessages();
}   
function prepareEditor(textarea) {
 return textarea.tinymce({
                          script_url : '/tiny_mce/tiny_mce.js',
                          theme : "advanced",
                          theme_advanced_toolbar_location : "top",     
                          theme_advanced_buttons1 : "bold,italic,underline,separator,strikethrough,bullist,numlist,separator,undo,redo,separator,cut,copy,paste",
                          theme_advanced_buttons2 : "",
                          convert_urls : false,
                          theme_advanced_toolbar_align : "left"
                        })}
 
standardDialogMask = "#333333"
    
$.tools.validator.addEffect("failWithFlashOnEmail", function(errors, event) {
	var invalidEmailErrMsg = "Du har inte skrivit in en e-post eller e-posten är felaktig. Vänligen försök igen.";
	var emptyEmailErrMsg = "Du måste ange e-post till motpart.";
	$.each(errors, function(index, error) {
    	var input = error.input;
        $(input).addClass("redborder");
        if (!$(input).hasClass("noflash")) addFlashMessage(invalidEmailErrMsg);
	});

}, function(inputs)  {
	$(inputs).removeClass("redborder");
});    


function swedishList(list)
{
  var res = strong(list[0]);
  for(i=1;i<list.length;i++)
  {
   if (i==list.length-1)
     res += " och " + strong(list[i])
   else
     res += ", " + strong(list[i])
     
  }
  return res;
}
function strong(l) {return "<strong>"+l+"</strong>"}

