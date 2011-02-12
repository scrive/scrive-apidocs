
if( !window.console ) {
    window.console = 
        { log: function() {}
        }
}

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
    return;
}

function enableInfoTextOnce(where)
{
    if( where==null ) {
        where = $(document);
    }
    var selector = ':input[infotext]';
    var inputs = $(selector, where);
    
    inputs.live("focus", function() { 
            if( $(this).hasClass("grayed")) {
                $(this).val("");
                $(this).removeClass("grayed");
            }
        });
    inputs.live("blur", function() {
            if( $(this).val()=="" || $(this).val()==$(this).attr("infotext") ) {
                $(this).addClass("grayed");
                $(this).val($(this).attr("infotext"));
            }
        });
    inputs.blur();

    $("form").live("submit",function () {
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

/* 
 * make edit bar stay at the top
 */
$(function () {
  var menu = $('#signStepsContainer.follow');
  if(menu.size() > 0) {
    var pos = menu.offset();
    var signStepsWrapper = $("#signStepsWrapper");
    var documentBox = $("#documentBox");
    $(window).scroll(function(){
      if($(this).scrollTop() >= pos.top && !menu.hasClass('fixed')){
        signStepsWrapper.height(menu.height() - 53);
        menu.addClass('fixed');
      } else if($(this).scrollTop() < pos.top && menu.hasClass('fixed')){
        signStepsWrapper.height(menu.height());
        menu.removeClass('fixed');
      }
    });
  }
});

$(function () {

	// remember the last row of the table that was clicked
        var focused;
        // two alternative ways to track clicks off of a .check
        //$("*").not(".check").click(function(){if(this == document.activeElement) { focused = null; }});
        // this way is cleaner since it only installs a handler on the toplevel document instead of nearly
        // every element

	// when you click outside of the table, lose focus
        $(document).click(function(event){ 
                if($(event.target).parents("#selectable").size() === 0){ 
                    focused = null; 
                }
            });

	// override click when shift key is held
        $("#selectable tr").mousedown(function(event){
                if(focused && event.shiftKey && $(event.target).filter(".check").size() === 0){
                    var checks = $(".check");
                    var startIndex = focused?checks.index(focused):null;
                    var endIndex = checks.index($(this).find(".check"));
                    
                    var s = Math.min(startIndex, endIndex);
                    var e = Math.max(startIndex, endIndex);
 
                    checks.slice(s, e + 1).attr("checked", true);

                    checks.not(":checked").parents("tr").removeClass("ui-selected");
                    checks.filter(":checked").parents("tr").addClass("ui-selected");

                    focused = $(checks.get(endIndex));
                    checks.get(endIndex).focus();

                    // cancel all further click processing
                    // we are overriding the Selectable behavior for shift-clicks
                    return false;
                }                     
            });
        $(".multiFileInput").each(
            function() {
               var upload = $(this);
               var form = this.form;
               $(this).MultiFile({
                  list: upload.attr("rel"),
                  onFileAppend: function() { 
                     if (upload.hasClass("submitOnUpload")) $(form).submit() 
                  }
                 
              })
              
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
    flashSpecialFlashMessages();

    /*
    $('#all').click(function() {
    });
    */
    enableInfoTextOnce();
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


    
  $("#sendinvite").overlay({
    mask: standardDialogMask,
    onBeforeLoad: function(){
      if (!emailFieldsValidation($(".stepForm input[type='email']"))) return false;
      if (!nonZeroSignatories()) return false;
      if (!authorFieldsValidation()) return false;

      var mrxs = $("form input[name='signatoryname']");
      var tot = "";
      var allparties = new Array();
      mrxs.each(function(index) { 
        allparties.push($(this).val());
      });
      tot = swedishString(allparties);
      $(".Xinvited").html(tot);
    }});

    $("#signinvite").overlay({  
    mask: standardDialogMask,    
    onBeforeLoad: function () { 
      if (!emailFieldsValidation($(".stepForm input[type='email']"))) return false;
      if (!nonZeroSignatories()) return false;
      if (!authorFieldsValidation()) return false;

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
            var signedList =  jQuery(".authornamewhennotsecretary");                                                     
            var authorSignes = jQuery("#authorsignatoryradio:checked").size()>0;
            var newtxt = $("#invitetext").val()
            $("#edit-invite-text-dialog textarea").val(newtxt);    
            var author = $(".authorname .fieldvalue").text();
            var sigs = $("#personpane .persondetails");
            var partners = new Array()
            var i = 0;
            if (authorSignes)   {
              signedList.html(signedList.attr("okprefix")+" <strong>"+author+"</strong>");
              partners[i] = author;
              i++;
            } else {
               signedList.html(signedList.attr("alt"));
            }
            //ignore first one (it is author and we added him earlier)
            sigs.slice(1).each(function(){
              var namefield = $("[name='signatoryname']",this);
              var mailfield =  $("[name='signatoryemail']",this)
              
              var res = namefield.val();
              if (!namefield.hasClass("grayed"))          
                 res = namefield.val();
              else if (!mailfield.hasClass("grayed") )
                 res = mailfield.val(); 
              partners[i] = res
              i++;
            })
            $(".partylistupdate").html(swedishList(partners));
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
        onBeforeLoad: function () {
               if (!sigFieldsValidation()) return false;
               var guardChecked = $(".signGuard:checked").size()>0;
               if (!guardChecked) 
               { $(".signGuard").parent().css("border","1px dotted red");
                 $(".signGuard").change(function(){$(this).parent().css("border","")});
                 //addFlashMessage("need text");
                 return false;
               } 
        }
    })

	$("#signbankid").overlay({ mask: standardDialogMask
		    });
    
    $("#cancel, .cancel, #signByAuthor").overlay({	mask: standardDialogMask    });
    
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
   $(".flashOnOn").change( function(){
     if ($(this).val()=="off")
       $(".flashMessage",$(this).parent()).each(function(){
            addFlashMessage($(this).html())
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

    showProperSignButtons();    
    $(".partyrole input").change(showProperSignButtons);

    function gettext(id) {
	return $("#" + id).text();
    }
    
    $(".submitStepFormOnClick").click( function(){
       $(".stepForm").submit(); })
    $(window).resize();
    
    //var rpxJsHost = (("https:" == document.location.protocol) ? "https://" : "http://static.");
    //document.write(unescape("%3Cscript src='" + rpxJsHost +
    //           "rpxnow.com/js/lib/rpx.js' type='text/javascript'%3E%3C/script%3E"));
    //    RPXNOW.overlay = true;
    //    RPXNOW.language_preference = 'sv';
});

function showProperSignButtons() {
  var sigfields = $(".dragfield").filter(function() {
    var field = $(this);
    var dragstatus = getDragStatus(field);
    var fillstatus = getFillStatus(field);

    if(!isStandardField(field) && dragstatus === 'placed' && fillstatus === 'sig') {
      return true;
    } else {
      return false;
    }
  });

  if(sigfields.size() > 0) {
    // we're awaiting author mode
    $("#signinvite").hide();
    $("#sendinvite").show();
  } else if($("#authorsignatoryradio").attr("checked")) {
    $("#signinvite").show();
    $("#sendinvite").hide();
    
  } else {
    $("#signinvite").hide();
    $("#sendinvite").show();
  }

}

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
  var dragfields = $("#personpane .dragfield");

  // get all the fields that should be filled by author
  var remainingAuthorFields = dragfields.filter(function() {
    return getFillStatus($(this)) === 'author';
  });

  if(remainingAuthorFields.size() > 0) {
    console.log(remainingAuthorFields);
    if(remainingAuthorFields.hasClass('signame')) {
      addFlashMessage("Du har inte skrivit in något namn på en eller flera motparter. Vänligen försök igen.");
    }
    if(remainingAuthorFields.hasClass('customfield')) {
      addFlashMessage("Du har inte namngett alla fält. Vänligen försök igen.");
    }
    remainingAuthorFields.addClass('redborder');
    return false;
  }

  var remainingDragFields = dragfields.filter(function() {
    return getDragStatus($(this)) === 'must place';
  });

  if(remainingDragFields.size() > 0) {
    var dragMsg = "Du har inte lagt till alla skapade fält i dokumentet. Vänligen försök igen.";
    addFlashMessage(dragMsg);
    remainingDragFields.addClass('redborder');
    return false;
  }

  return true;
}

function sigFieldsValidation(){
  var remainingSigFields = $(".dragfield").filter(function() {
    var field = $(this);
    if(getValue(field).length === 0) {
      return true;
    }
  });

  if(remainingSigFields.size() > 0) {
    addFlashMessage("Var vänlig och fyll i all fält.");
    remainingSigFields.addClass("redborder");
    return false;
  } else {
    return true;
  }
}

function nonZeroSignatories() {
  var sigs = 0;
  if($("#authorsignatoryradio").attr("checked")) {
    sigs = 1;
  }
  
  // sum up all signatories (but minus author because we already
  // counted him)
  sigs += $("#personpane .persondetails").length - 1;

  var error = (sigs === 0);

  if(error) {
    addFlashMessage('Det finns inga undertecknande parter för detta dokument. Vänligen lägg till undertecknande parter eller ändra din roll till "undertecknare".');
    $("li.plus").addClass("redborder");
    $(".authordetails .man").addClass("redborder");
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

function flashSpecialFlashMessages(){
    var flashmsgbox = $('#signViewNotificationContainer');
    flashmsgbox.show();
    flashmsgbox.delay(12000).fadeOut();
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
                          theme_advanced_toolbar_align : "left",
                          plugins : "noneditable"
                                                          
                        })}
 
standardDialogMask = "#333333"
    
$.tools.validator.addEffect("failWithFlashOnEmail", function(errors, event) {
  var invalidEmailErrMsg = "Du har inte skrivit in en e-post eller e-posten är felaktig. Vänligen försök igen.";
  var emptyEmailErrMsg = "Du måste ange e-post till motpart.";
  $.each(errors, function(index, error) {
    var input = error.input;
    $(input).parents('.inputWrapper').addClass("redborder");
    if(!$(input).hasClass("noflash")) {
      addFlashMessage(invalidEmailErrMsg);
    }
  });
}, function(inputs)  {
  $(inputs).parents('.inputWrapper').removeClass("redborder");
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

function showStep1()
{
    $('#step1select').addClass("current");
    $('#step2select').removeClass("current");
    $('#step3select').removeClass("current");
    $('#signStep1Content').show();
    $('#signStep2Content').hide();
    $('#signStep3Content').hide();
    $('#signStepsNextButton').show();
    return false; 
}

function showStep2()
{
    $('#step1select').removeClass("current");
    $('#step2select').addClass("current");
    $('#step3select').removeClass("current");
    $('#signStep1Content').hide();
    $('#signStep2Content').show();
    $('#signStep3Content').hide();
    $('#signStepsNextButton').show();
  
  $("#signStepsWrapper").height($('#signStepsContainer.follow').height());

    return false;
}

function showStep3()
{
    $('#step1select').removeClass("current");
    $('#step2select').removeClass("current");
    $('#step3select').addClass("current");
    $('#signStep1Content').hide();
    $('#signStep2Content').hide();
    $('#signStep3Content').show();
    $('#signStepsNextButton').hide();
  showProperSignButtons();
    return false;
}

function nextStep()
{
    console.log("next step");
    if( $('#signStep1Content').is(':visible')) {
        showStep2();
    }
    else if( $('#signStep2Content').is(':visible')) {
        showStep3();
    }
    return false;
}

function checkPersonPaneMode()
{
    var personpane = $('#personpane');
    var signStepsBody = $("#signStepsBody");
	
    if( personpane.children().size()>2 ) {
        signStepsBody.removeClass("personPaneSmallNumberMode");
        signStepsBody.addClass("personPaneLargeNumberMode");
    } else {
        signStepsBody.addClass("personPaneSmallNumberMode");
        signStepsBody.removeClass("personPaneLargeNumberMode");
    }
}


$(document).ready(function() {
        $('#step1select a').click(showStep1);
        $('#step2select a').click(showStep2);
        $('#step3select a').click(showStep3);
        $('#signStepsNextButton').click(nextStep);

        $('a', '#peopleList').live('click', function() {
                var li = $(this).parent();
                var ol = li.parent();
                var idx = ol.children().index(li);
                var children = $('#personpane').children();
                children.filter(':not(:eq(' + idx + '))').removeClass("currentPerson");
                children.filter(':eq(' + idx + ')').addClass("currentPerson");
                return false;
            });

        $('#delSignatory').click(function() {
                var personpane = $('#personpane');
                var children = personpane.children();
                var child = children.filter('.currentPerson');
          if(child.hasClass("authordetails")) {
            return false;
          }
                var idx = children.index(child);

		//console.log(child);
		var sigid = getHiddenField(child, "sigid");
                
		//console.log("removing signatory with id: " + sigid);

		detachFieldsForSig(sigid);
                child.remove();
                
                var li = $("#peopleList li:eq(" + idx + ")");
                li.remove();

		var newidx = idx;
		if(newidx >= $('#personpane').children().size() ) {
		    newidx = $('#personpane').children().size()-1;
		}
                
		personpane.children(".persondetails:eq(" + newidx + ")").addClass("currentPerson");

          personpane.children().each(function(idx) {
            var p = $(this);
            p.find(".partnumber").html("PART " + (idx + 1));
          });
                checkPersonPaneMode();
            });
        $("input[name='signatoryname']", "#personpane").live('change keyup', function() {
                var val = $(this).val();
                //console.log(val);
                var div = $(this).parentsUntil("#personpane").last();
                var idx = div.parent().children().index(div);
                if( val=="") 
                    val = "(fill in)";
                $('#peopleList li:eq(' + idx + ') a').text(val);
            });
     });

/*
 * The link to add a signatory.
 *  - add a new signatory to #personpane
 *  - make the last one the .currentPerson
 *  - check and possibly change the mode (2 person/list mode)
 *  - Renumber all of the parts
 *  - Enable info text for the new part
 *  - Remove the .redborder from the addSignatory button
 *  - Remove the .redborder from the author's man button
 *  - Disable more event processing
 */
$(function() {
  var addsignatory = $('#addSignatory');
  var personpane = $('#personpane');
  var authorman = $(".authordetails .man");
  // where the red border appears for
  // addsig button
  var liplus = $("li.plus"); 

  addsignatory.click(function() {
    signatoryToHTML(newsignatory());
    var children = personpane.children();
    var newone = children.removeClass("currentPerson").last().addClass("currentPerson");
    checkPersonPaneMode();
          
    children.each(function(idx) {
      var p = $(this);
      p.find(".partnumber").html("PART " + (idx + 1));
    });

    enableInfoTextOnce(newone);
    liplus.removeClass("redborder");
    authorman.removeClass("redborder");
    return false;
  });
});

/*
 * When the author selects signatory, we have to 
 * remove the redborder for the validation error
 * where there is only zero signatories.
 */
$(function() {
  var authorman = $(".authordetails .man");
  // where the red border appears for
  // addsig button
  var liplus = $("li.plus");
  $("#authorsignatoryradio").click(function() {
    authorman.removeClass('redborder');
    liplus.removeClass('redborder');
  });
});
