if(!window.console) {
  window.console = { 
    log: function() {}
  };
}

function safeReady(f) {
  $(function() {
    try {
      f();
    } catch(e) {
      console.log(e);
    }
  });
}

function repeatForeverWithDelay(delay) {
  return function () {
    var that = this;
    $(document).delay(delay).queue(function() {
      $(this).dequeue();
      $.ajax(that);
    });
  };
}

function getUniqueId() {
    var rnd = Math.round(Math.random() * 1000000000);
    while($("#" + rnd).length  > 0) {
        rnd = Math.round(Math.random() * 1000000000);
    }
    return rnd;
}

function enableInfoTextOnce(where){
  if(!where) {
    where = $(document);
  }
  var selector = 'input[infotext] ,  textarea[infotext]';
  var inputs = $(selector, where);
  
  function setInfoText(obj) {
    var input = $(obj);
    if (input.val() == "" || input.val() == input.attr("infotext")) {
       input = input.not($(document.activeElement));
       input.addClass("grayed");
       input.val(input.attr("infotext"));
    }
  };
  
  inputs.live("focus", function() { 
    var input = $(this);
    if(input.hasClass("grayed")) {
      input.val("");
      input.removeClass("grayed");
    }
  });
  
  inputs.live("blur", function() {
    setInfoText(this);
  });
  
  inputs.each(function() {
    setInfoText(this);
  });
  
  $("form").live("submit", function () {
    var elems = $(this).find(".grayed");
    elems.val("");
    return true;
  });
}

// not used
function disableInfoText(where) {
  if(!where) {
    where = $(document);
  }
  inputs = where.filter('input[infotext] ,  textarea[infotext]');
  inputs.focus();
}

function swedishString(names) { 
  if(names.length === 0) {
    return "";
  }
  if(names.length === 1) {
    return "<strong>" + names[0] + "</strong>";
  }
  
  var name0 = names.shift();  
  if(names.length === 1) {
    return "<strong>" + name0 + "</strong> och " + swedishString(names);
  }

  return "<strong>" + name0 + "</strong>, " + swedishString(names);
}

/* 
 * make edit bar stay at the top
 */
safeReady(function () {
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

/*
 * For delete confirmation on lists.
 */
safeReady(function() {
  $(".listDelete").overlay({
    mask: standardDialogMask,
    onBeforeLoad: function() {
      var selectedrows = $(".listForm tbody tr.ui-selected");
      if (selectedrows.length==0) {
        return false;
      } else {
        var sentoropencount = selectedrows.find(".sent").length + selectedrows.find(".opened").length
        if (sentoropencount>0) {
          var listtype = jQuery.trim($(".listForm").find(".listtype").text().toLowerCase());
          if (listtype=="dokument") {
            addFlashMessage("Det går inte att radera dokument som är skickade eller öppna, var vänliga återkalla dokumentet först.", "red");
          } else {
            addFlashMessage("Det går inte att radera offer som är skickade eller öppna, var vänliga återkalla offerter först.", "red");
          }
          return false;
        }
        var deletionDetails = "";
        if (selectedrows.length==1) {
          deletionDetails = jQuery.trim(selectedrows.find(".listname").text());
        } else {
          var listtype = jQuery.trim($(".listForm").find(".listtype").text().toLowerCase());
          deletionDetails = selectedrows.length + " " + listtype;
        }
        $("#dialog-list-delete-confirm").find(".deletionDetails").text(deletionDetails);
        return true;
      }
    }
  });
});

safeReady(function() {
  $(".listRemind").overlay({
    mask: standardDialogMask,
    onBeforeLoad: function() {
      var selectedrows = $(".listForm tbody tr.ui-selected");
      if (selectedrows.length==0) {
        return false;
      } else {
        var sentoropencount = selectedrows.find(".sent").length + selectedrows.find(".opened").length;
        if (sentoropencount!=selectedrows.length) {
          var listtype = jQuery.trim($(".listForm").find(".listtype").text().toLowerCase());
          addFlashMessage("Det går inte att skicka påminnelser för " + listtype + " som inte är skickade eller öppna.", "red");
          return false;
        }
        var singlemsg = $("#dialog-list-remind-confirm").find(".singleremindmsg");
        var multiplemsg = $("#dialog-list-remind-confirm").find(".multipleremindmsg");
        if (selectedrows.length==1) {
          var docname = jQuery.trim(selectedrows.find(".listname").text());
          singlemsg.find(".docname").text(docname);
          singlemsg.show();
          multiplemsg.hide();
        } else {
          multiplemsg.find(".doccount").text(selectedrows.length);
          singlemsg.hide();
          multiplemsg.show();
        }
        return true;
      }
    }
  });
});

/*
 * For the arkiv view fancy selection
 */
safeReady(function () {
  var selectables = $("#selectable");
  var checks = $(".check");
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
  // cannot be live or it doesn't override properly
  $("tr", selectables).mousedown(function(event){
    // we have a previous focus
    // shift key is pressed
    // check is not clicked
    if(focused && event.shiftKey && $(event.target).filter(".check").size() === 0){
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


  // the jQuery Selectable feature 
  selectables.selectable({
    // links and input fields do not have click overridden
    cancel: 'a,input',
    
    unselected: function(event, ui) {
      $(ui.unselected).find(".check").attr("checked", false);
    },

    selected: function(event, ui) {
      focused = $(ui.selected).find(".check")
        .attr("checked", true)
        .focus();
    }});

  $(".check:checked").parents("tr").addClass("ui-selected");
  $(".ui-selected").find(".check").attr("checked", true);
        
  $(".check", selectables).live("click", function(event) {
    // shift
    // have a saved focus
    // focused is a check
    // focused is checked
    if(event.shiftKey && focused && focused.filter(".check").size() > 0 && focused.attr("checked")){
      var startIndex = checks.index(focused);
      var endIndex = checks.index(this);
      
      var s = Math.min(startIndex, endIndex);
      var e = Math.max(startIndex, endIndex);
      
      var checksslice = checks.slice(s, e+1);
      checksslice.attr("checked", true);
      checksslice.parents("tr").addClass("ui-selected");
    } else {
      if($(this).attr("checked")) {
        $(this).parents("tr").addClass("ui-selected");
      } else {
        $(this).parents("tr").removeClass("ui-selected");
      }
    }

    focused = $(this);
  });

  $('#all').live("click", function() {
    var acc = checks.filter(function() {
      return $(this).attr("checked");
    }).size() > 0;

    checks.attr("checked", !acc);
    
    if(!acc) {    
      checks.parents("tr").addClass("ui-selected");
    } else {
      checks.parents("tr").removeClass("ui-selected");
    }
  }); 
});

function initFileInputs(){
  $(".multiFileInput").each(function() {
    var upload = $(this);
    var form = $(this).parents("form");
    upload.MultiFile({
      list: upload.attr("rel"),
      onFileAppend: function() { 
        displayLoadingOverlay("Laddar upp . . .");
        if (upload.hasClass("submitOnUpload")) {
          form.submit();
        }
      }
    });
  });   
}

safeReady(function() {
 initFileInputs();
});

safeReady(function() {
  flashFlashMessages();
  flashSpecialFlashMessages();
  showModal();
  enableInfoTextOnce();
});

safeReady(function() {
  if(typeof(window.documentid) != "undefined") {
    var myurl;
    if (typeof(window.siglinkid) != "undefined"
    &&  typeof(window.sigmagichash) != "undefined")
        myurl = "/pagesofdoc/" + documentid + "/" + siglinkid + "/" + sigmagichash;
    else
        myurl = "/pagesofdoc/" + documentid;
    $.ajax({ url: myurl,
             success: function(data) {
               var content = $(data);
               var errormsg = content.find(".errormsg")
               if (errormsg.length > 0) {
                   var errdialog = $("<div title='Problem med PDF'>"
                                   + errormsg.text()
                                   + "</div>");
                   errdialog.dialog({
                       open: function(event, ui) { $(".ui-dialog-titlebar-close").hide(); },
                       modal: true,
                       closeOnEscape: false,
                       resizable: false,
                       minWidth: 400,
                       buttons: {
                           Back : function() {
                               window.location.href='/d';
                           }
                       }
                   });
               } else {
                   $('#documentBox').html(content);
               }
             },
             error: repeatForeverWithDelay(1000)
           });
  }
});

function escapeHTML(s) {
    var result = '';
    for (var i = 0; i < s.length; ++i) {
        if (s[i] == '&')
            result += '&amp';
        else if (s[i] == '\'')
            result += '&#39;';
        else if (s[i] == '"')
            result += '&quot;';
        else if (s[i] == '<')
            result += '&lt;';
        else if (s[i] == '>')
            result += '&gt;';
        else
            result += s[i];
    }
    return result;
}

function allparties()
{
      var sigs = $("form .persondetails").filter(":not(.authordetails)");
      var allparties = new Array();
      sigs.each(function(index) { 
              if( $("input:radio:first:checked",this).length>0 ) {

                  var fstnameelem = $("input[name='signatoryfstname']", this);
                  if (isMultiPartElem(fstnameelem)) {
                      allparties.push(csvrowcount + " Parter");
                  } else {
                      var fstname = fstnameelem.val();
                      var sndname = $("input[name='signatorysndname']",this).val();  
                      allparties.push(escapeHTML(fstname + " " + sndname));
                  }
              }
          });
      return allparties;   
}

safeReady(function() {
  $("#sendinvite").overlay({
    mask: standardDialogMask,
    onBeforeLoad: function(){
      if (isInvalidCSV()) return false;
      if (!emailFieldsValidation(noMultiParts($(".stepForm input[type='email']")))) return false;
      if (!nonZeroSignatories()) return false;
      if (!authorFieldsValidation()) return false;
      if (!checkSignatoriesHaveUniqueEmail()) return false;
      if (!checkAllCustomFieldsAreNamed()) return false;
      fieldValidationType = "";
      var tot = swedishString(allparties());
      $(".Xinvited").html(tot);
    }
  });

  $("#signinvite").overlay({  
    mask: standardDialogMask,    
    onBeforeLoad: function () { 
      if (!checkSignPossibility()) return false;
      if (isInvalidCSV()) return false;
      if (!emailFieldsValidation(noMultiParts($(".stepForm input[type='email']")))) return false;
      if (!nonZeroSignatories()) return false;
      if (!authorFieldsValidation()) return false;
      if (!checkSignatoriesHaveUniqueEmail()) return false;
      if (!checkAllCustomFieldsAreNamed()) return false;
      fieldValidationType = "";
      var tot = swedishString(allparties());
      $(".Xinvited").html(tot);
    }
  });
});

safeReady(function() {
  $("#tobasic").overlay({
    mask: standardDialogMask
  });

  $("#toadvanced").overlay({
    mask: standardDialogMask
  });

  $("#dialog-confirm-basic .tobasic").click(function() {
    $("form input[type='hidden'][name='docfunctionality']").attr("value", "BasicFunctionality");
    return true;
  });

  $("#dialog-confirm-advanced .toadvanced").click(function() {
    $("form input[type='hidden'][name='docfunctionality']").attr("value", "AdvancedFunctionality");
    return true;
  });
});

function checkSignatoriesHaveUniqueEmail() {
  var isRepetition = false;
  var emails = $(".stepForm").find("input[type='email']");
  emails.each(function() {
    var emailvalue = $(this).val();
    var matchingemails = emails.filter(function() {
      return $(this).val()==emailvalue;
    });
    if (matchingemails.length>1) {
      matchingemails.closest("span").addClass("redborder");
      isRepetition = true;
    }
  });
  
  var isAuthorUsedAsSignatory = false;
  var authoremail = $(".stepForm .authoremail .fieldvalue").text();
  var emailsmatchingauthor = emails.filter(function() {
    console.log($(this).val());
    console.log("=");
    console.log(authoremail);
    return $(this).val()==authoremail;
  });
  if (emailsmatchingauthor.length>0) {
    emailsmatchingauthor.closest("span").addClass("redborder");
    isAuthorUsedAsSignatory = true;
  }
  
  if (isRepetition || isAuthorUsedAsSignatory) {
    addFlashMessage("Du kan ej använda samma e-post för mer än 1 användare", "red");
    return false;
  } else {
    return true;
  }
}
safeReady(function() {
  $(".submiter").live("click", function(){
    $(this).parents("form").submit();
  });
});

// bankid stuff
/*
safeReady(function() {
  $("#dialog-confirm-sign-eleg .bankid").click(function(){
    sign2(window.location.pathname,
	  "#dialog-confirm-sign-eleg",
	  "/bankid" + window.location.pathname);
    return false;
  });
});
*/

safeReady(function() {
  $(".editer").live("click", function(){
    prepareForEdit($(this).parents("form"));
    $(this).hide();
    return false;
  })
});

safeReady(function() {
  $("#editinvitetextlink").overlay({        
    mask: standardDialogMask,    
    onBeforeLoad: function () { 
      var signedList =  jQuery(".authornamewhennotsecretary");
      var authorSignes = jQuery("#authorsignatoryradio:checked").size() > 0;
      var newtxt = $("#invitetext").val();
      $("#edit-invite-text-dialog textarea").val(newtxt);    
      var author = $(".authorname .fieldvalue").text();
      var sigs = $("#personpane .persondetails");
      var partners = new Array();

      if (authorSignes)   {
        signedList.html(signedList.attr("okprefix")+" <strong>"+author+"</strong>");
        partners.push(author);
        i++;
      } else {
        signedList.html(signedList.attr("alt"));
      }
      //ignore first one (it is author and we added him earlier)
      sigs.slice(1).each(function(){
              if( $("input:radio:first:checked",this).length>0 ) {
                  var fstnamefield = $("input[name='signatoryfstname']",this);
                  var sndnamefield = $("input[name='signatorysndname']",this);
                  var mailfield =  $("input[name='signatoryemail']",this)
                      var res = escapeHTML(fstnamefield.val() + " " + sndnamefield.val());
                  if ((!mailfield.hasClass("grayed")) && 
                      (fstnamefield.hasClass("grayed") && 
                       sndnamefield.hasClass("grayed")))
                      res = mailfield.val(); 
                  partners.push(res);
              }
          });
      $(".partylistupdate").html(swedishList(partners));
    }
  });   
});

safeReady(function() {
  $("#editing-invite-text-finished").click(function() {
    var newtxt = $("#edit-invite-text-dialog textarea").val();
    $("#invitetext").val( newtxt );
  });
});
   
safeReady(function() {                      
  $(".redirectsubmitform").submit(function(){
    var newform = $($(this).attr("rel"));
    var inputs = $("input",$(this));
    $('textarea:tinymce',$(this)).each(
      function(){
        inputs = inputs.add($("<input name='"+$(this).attr('name')+"' value='"+$(this).html()+"'>"))
      });
    inputs.css("display","none");
    newform.append(inputs); 
    newform.submit();
    return false; 
  });
});

safeReady(function() {                      
  $("#sign").overlay({ mask: standardDialogMask,
                       onBeforeLoad: function () {
                         if (!sigFieldsValidation()) return false;
                         var guardChecked = $(".signGuard:checked").size()>0;
                         if (!guardChecked) { 
                           $("#signGuardLabel").css("border","1px dotted red");
                           $(".signGuard").change(function(){$("#signGuardLabel").css("border","")});
                           if (typeof(offer) !== "undefined" && offer)
                               addFlashMessage("För att bekräfta måste du först klicka i kryssrutan", "red");
                           else 
                               addFlashMessage("För att underteckna måste du först klicka i kryssrutan", "red");
                           return false;
                         } 
                       }
                     });
});

safeReady(function() {
  $("#signbankid").overlay({ mask: standardDialogMask });
});
   
safeReady(function() { 
  $("#cancel, .cancel").overlay({	mask: standardDialogMask    });
});

safeReady(function() {
  $("#signByAuthor").overlay({	
    mask: standardDialogMask,
    onBeforeLoad: function () {
      if (!sigFieldsValidation()) return false;
      var guardChecked = $(".signGuard:checked").size()>0;
      if (!guardChecked) 
      { $(".signGuard").parent().css("border","1px dotted red");
        $(".signGuard").change(function(){$(this).parent().css("border","")});
        //addFlashMessage("need text");
        return false;
      } 
    }});
});
   
safeReady(function() { 
  $("#toscontainer").overlay({
    mask: standardDialogMask,
    load: true
  });
});

safeReady(function() {	
  $("#loginbtn").click(function(){
    if(emailFieldsValidation($(":email", $(this).parents("form")))) {
      $($(this).parents("form")).submit();
    }						  
    return false;
  });
});

safeReady(function() {
  $("#createnewaccount").click(function(){
    if(emailFieldsValidation($("input[type='email']"))){
      $($(this).parents("form")).submit();
    }
    return false;
  });
});
   
safeReady(function() { 
  $(".validateEmail").click(function(){
    return (emailFieldsValidation($(":email",$(this).parents("form"))))
  });
});

safeReady(function() {  
  $(".flashOnClick").click(function(){
    $(".flashMessage", $(this).parent()).each(function(){
      addFlashMessage($(this).html(),$(this).attr("flashtype"));
      $(this).remove();
    });
  });
  
  $(".flashOnOn").change(function(){
    var ftype = $(this).attr("flashtype");  
    if($(this).val() == "off") {
      $(".flashMessage", $(this).parent()).each(function(){
        addFlashMessage($(this).html(),ftype);
      });
    }
  });
});

safeReady(function() {
  $(".addremovecheckbox").change(function(){
    var what = $($(this).attr("rel"));
    var location = $($(this).attr("location"));
    var oldlocation = $($(this).attr("oldlocation"));
    if($(this).val() == "off") {  
      location.append(what);      
      $(this).val("on"); 
      $(this).attr("checked", "checked");
    } 
    else {
      oldlocation.append(what);
      $(this).val("off");
      $(this).removeAttr("checked");
    }    
    return true;
  }).each(function(){
    if($(this).val() == "on") {
      $($(this).attr("location")).append($($(this).attr("rel")));      
      $(this).attr("checked","checked");
    } 
  });    
});

safeReady(function() {
  $(".switchercheckbox").change(function(){
    var on = $($(this).attr("on"));
    var off = $($(this).attr("off"));

    var checked = $(this).attr("checked");

    if(checked) {
      off.hide();
      on.show();
    } else {
      on.hide();
      off.show();
    }
/*
    if($(this).val() == "off") {  
      off.hide();
      on.show();
      $(this).val("on"); 
      this.checked = true;
    
    } 
    else {
        on.hide();
        off.show();
        $(this).val("off");
        this.checked = false;
    }    
    return false;*/
  });
});

safeReady(function() {    
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
        $(".datehere",curr).text("("+curr.data("dateinput").getValue('dd-mm-yy')+")"); 
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
});

safeReady(function() {
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
    if(input.val() == "") {
      curr.data("dateinput").setValue(new Date());
    } else {  
      date = input.val().split('-');
      curr.data("dateinput").setValue(date[0],date[1] -1,date[2]); 
    }
  });
});

safeReady(function() {
  $(".replacebynextonclick").click( function(){
    var replacement = $(this).next();
    $(this).replaceWith(replacement);
    replacement.show();
  });
});

safeReady(function() {
  $(window).resize();
});

function deactivateSigninvite(){
  var checkBox = $(".sendcheckbox");
  checkBox.attr("DISABLED","");
}

function activateSignInvite(){
  var checkBox = $(".sendcheckbox");
  checkBox.removeAttr("DISABLED");
}
function showProperSignButtons() {
  var checkBox = $(".sendcheckbox");
  var numsigs = $("#personpane .persondetails").length;
  if(numsigs > 1) {
    if($("#authorsignatoryradio").attr("checked")) {
      activateSignInvite();
      checkBox.parent().find(".usual").show();
      checkBox.parent().find(".secretary").hide();
    } else {
      if(!checkBox.attr("checked")) { 
        checkBox.attr("checked", true).change();
      }   
      deactivateSigninvite();
      checkBox.parent().find(".usual").hide();
      checkBox.parent().find(".secretary").show();
    }
  } else {
    if(checkBox.attr("checked")) { 
      checkBox.attr("checked", false).change();
    }   
    checkBox.parent().find(".usual").show();
    checkBox.parent().find(".secretary").hide();
    deactivateSigninvite();
  }

  if($("#authorsecretaryradio").attr("checked")) {
    $("#dialog-confirm-text-send").show();
    $("#dialog-confirm-text-send-fields").hide();
    $("#dialog-confirm-text-send-normal").hide();
  } else {
    // normal
    $("#dialog-confirm-text-send").hide();
    $("#dialog-confirm-text-send-fields").hide();
    $("#dialog-confirm-text-send-normal").show();

  }
}

function emailFieldsValidation(fields){
  fields.removeClass("noflash");
  fields = fields.filter(function(){return !isExceptionalField($(this))});
  if(fields.length > 0){
    var inputs = fields.validator({
      effect:'failWithFlashOnEmail',
      formEvent: 'null'
    });
    var valid = inputs.data("validator").checkValidity();
    if(!valid) {
      fieldValidationType = "email";
    }
    return valid;
  }
  fieldValidationType = "";
  return true;
}

function checkSignPossibility() {
  if($("#authorsecretaryradio").attr("checked")) {
    // secretary
    $(".authordetails .man").addClass("redborder");   
    addFlashMessage("Du kan inte underteckna när du är sekreterare. Om du vill underteckna, gå tillbaks till steg 2 och byt roll.","red");
    return false;
  } else {
    // sign is possible
    return true;
  }

  return true;
}

function checkAllCustomFieldsAreNamed() {

  unamedfields = $("#personpane .newfield");
  unamedfields.addClass("redborder");
  if (unamedfields.length>0) {
    addFlashMessage("Var vänlig namnge alla skapade fält", "red");
    return false;
  } else {
    return true;
  }
}

var fieldValidationType = "";
function authorFieldsValidation() {
  var dragfields = $("#personpane .dragfield");
  dragfields.removeClass('offending');
  // get all the fields that should be filled by author
  var remainingAuthorFields = dragfields.filter(function() {
    return getFillStatus($(this)) === 'author' && 
      !isMultiPartElem($(this)) &&
      $(this).parents(".sigentry").find(".partyrole input:radio[value=signatory]").attr("checked");
  });

  if(remainingAuthorFields.size() > 0) {
    console.log(remainingAuthorFields);
    if(remainingAuthorFields.hasClass('sigfstname') || remainingAuthorFields.hasClass('sigsndname')) {
      addFlashMessage("Du har inte skrivit in något namn på en eller flera motparter. Vänligen försök igen.","red");
    }
    if(remainingAuthorFields.hasClass('customfield')) {
      addFlashMessage("Du har inte namngett alla fält. Vänligen försök igen.","red");
    }
    if(remainingAuthorFields.hasClass('sigpersnum')) {
      addFlashMessage("Var vänlig gå tillbaka till steg 2 och fyll i personnummer.", "red");
    }
    remainingAuthorFields.addClass('redborder').addClass('offending');
    fieldValidationType = "fillstatus";
    return false;
  }

  fieldValidationType = "";
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
    addFlashMessage("Du måste fylla i tomma fält innan du kan underteckna.","red");
    remainingSigFields.addClass("redborder");
    return false;
  } else {
    return true;
  }
}

function isInvalidCSV() {
  return $("#personpane .persondetails .csvinvalid").length > 0;
}

function nonZeroSignatories() {
  var sigs = 0;
  // sum up all signatories  
  sigs += $("#personpane .persondetails input:hidden[name='signatoryrole'][value='signatory']").length;
  // add author if a signatory
  if($("#authorsignatoryradio").attr("checked")) {
    sigs++;
  }

  // sum up all signatories (but minus author because we already
  // counted him)
  sigs += $("#personpane .persondetails input:hidden[name='signatoryrole'][value='signatory']").length - sigs;

  var error = (sigs === 0);

  if(error) {
    addFlashMessage('Du måste ha minst en undertecknande part.',"red");
    $("li.plus").addClass("redborder");
    $(".authordetails .man").addClass("redborder");
    return false;
  }
  return true;
}

function isExceptionalField(field) {
  return (field.closest("div").attr("id") == "signatory_template");
}


safeReady(function(){
    $(".prepareToSendReminderMail").each(function(){
        $(this).overlay({
          mask: standardDialogMask 
        });
    });              
});

function prepareForEdit(form){
    $(".editable",form).each(function(){
      var textarea = $("<textarea style='width:470px;height:0px;border:0px;padding:0px;margin:0px'  name='"+$(this).attr('name')+"'> "+ $(this).html()+ "</textarea>")
      var wrapper = $("<div></div>").css("min-height",($(this).height())+15+"px");
      wrapper.append(textarea);
      $(this).replaceWith(wrapper);
      var editor = prepareEditor(textarea);
    });
    $(".replacebynextonedit",form).each(function(){
      var replacement = $(this).next();
      $(this).replaceWith(replacement);
      replacement.show();
    });
}

function flashSpecialFlashMessages(){
    var flashmsgbox = $('#signViewNotificationContainer');
    flashmsgbox.delay(12000).fadeOut();
}

function flashFlashMessages() {
    var flashmsgbox = $(".flashmsgbox");
    if ($(".flash-container", flashmsgbox).size() > 0) {
        // delay() and click() doesn't work correctly in document
        // creator since clearQueue() doesn't call clearTimeout(),
        // so we need to use setTimeout() and unregister set events
        // with clearTimeout() if user closed flash message by himself
        var event = setTimeout(hideFlashMessages, 10000);
        flashmsgbox.slideDown(800);
        flashmsgbox.click(function() {
            hideFlashMessages(event);
        });
    }
}

function showModal(){
   var modalbox = $(".modalbox");
   modalbox.overlay({
        mask: {
        color: standardDialogMask,
        loadSpeed: 0,
        opacity: 0.9
        },
        speed : 0
   });   
  if(modalbox.size() > 0) {
    modalbox.first().data("overlay").load();
  }
}
function hideFlashMessages(event) {
    if (event !== undefined)
        clearTimeout(event);
    $(".flashmsgbox").slideUp(800,function() {
        $(this).children().remove();
    });
}

function addFlashMessage(msg, type){
    var flashmsgbox = $('.flashmsgbox');    
    flashmsgbox.children().remove();
    flashmsgbox.append(
	  "<div class='flash-container " + type + "'>"
        + "<div class='flash-content'>"
		+ "<div class='skrivapa-logo float-left'></div>"
        + "<div class='flash-icon " + type + "'></div>"
        + "<div class='flash-body'>" + msg + "</div>"
		+ "<div class='flash-close modal-close'></div></div></div>"
    );
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
                          plugins : "noneditable,paste",
                          valid_elements : "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul"
 });
}
 
standardDialogMask = "#333333"
    
$.tools.validator.addEffect("failWithFlashOnEmail", function(errors, event) {
  var invalidEmailErrMsg = "Du har inte skrivit in en e-post eller e-posten är felaktig. Vänligen försök igen.";
  var emptyEmailErrMsg = "Du måste ange e-post till motpart.";
  $.each(errors, function(index, error) {
    var input = $(error.input);
    input.parents('.inputWrapper').addClass("redborder");
    if(!input.hasClass("noflash")) {
      addFlashMessage(invalidEmailErrMsg,"red");
      input.addClass("noflash");
    }
  });
}, function(inputs)  {
  $(inputs).parents('.inputWrapper').removeClass("redborder");
});    


function swedishList(list) {
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

function strong(l) {
  return "<strong>"+l+"</strong>";
}

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
  
  resizeDesignBar();

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

function nextStep() {
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
          resizeDesignBar();
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

		detachFieldsForSigID(sigid);
                child.remove();

                var csvpersonindex = personpane.closest("form").find("input[type='hidden'][name='csvpersonindex']"); 
                if (idx == csvpersonindex.attr("value")) {
                   personpane.closest("form").find("input[type='hidden'][name='csvpersonindex']").removeAttr("value");
                }
                
                var li = $("#peopleList li:eq(" + idx + ")");
                li.remove();

		var newidx = idx;
		if(newidx >= $('#personpane').children().size() ) {
		    newidx = $('#personpane').children().size()-1;
		}
                
		personpane.children(".persondetails:eq(" + newidx + ")").addClass("currentPerson");
          renumberParts();
                checkPersonPaneMode();
          resizeDesignBar();
            });
        $("input[name='signatoryfstname'], input[name='signatorysndname']", "#personpane").live('change keyup', function() {
                var fstname = $("input[name='signatoryfstname']",$(this).parent().parent());
                var sndname = $("input[name='signatorysndname']",$(this).parent().parent());
                if (fstname.hasClass("grayed"))
                  fstname = ""
                else fstname = fstname.val();
                if (sndname.hasClass("grayed"))
                  sndname = ""
                else sndname = sndname.val();
                var val = fstname + " "+ sndname;
                var div = $(this).parentsUntil("#personpane").last();
                var idx = div.parent().children().index(div);
                if (idx<0) {
                   return;
                }
                if(fstname == "" && sndname == "") {
                    val = "(Namnlös)";
                }
                if (isMultiPartElem($(this))) {
                    val = "Massutskick";
                }
                $('#peopleList li:eq(' + idx + ') a').text(val);
            });
        $('form.requestAccount').submit(function(){
            if (!emailFieldsValidation($("input[type='email']",$(this)))) return false;
                
        })
     });

function noMultiParts(elems) {
  return elems.filter(function() {
    return !isMultiPartElem($(this));
  });
}

function isMultiPartElem(elem) {
  var div = elem.closest(".persondetails");
  var idx = div.parent().children().index(div);
  var csvpersonindex = div.closest("form").find("input[type='hidden'][name='csvpersonindex']").attr("value");
  return (csvpersonindex==idx);
}

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
safeReady(function() {
  var addsignatory = $('#addSignatory');
  var personpane = $('#personpane');
  var authorman = $(".authordetails .man");
  // where the red border appears for
  // addsig button
  var liplus = $("li.plus"); 

  addsignatory.click(function() {
    signatoryToHTML(false, newsignatory());
    var children = personpane.children();
    var newone = children.removeClass("currentPerson").last().addClass("currentPerson");
    checkPersonPaneMode();
          
    renumberParts();

    enableInfoTextOnce(newone);
    liplus.removeClass("redborder");
    authorman.removeClass("redborder");

    resizeDesignBar();
    return false;
  });
});

function renumberParts() {
  updateCsvPersonIndex();

  console.log("renumber parts");
  var persondetails = $("#personpane .persondetails");

  var idx = 1;
  persondetails.each(function () {
          var authorrole = $(this).find("input:radio[value='signatory']:checked");
          var signatoryrole = $(this).find("input:radio[value='signatory']:checked");
          var isSignatory = (authorrole.length + signatoryrole.length)>0;
          if( isSignatory ) {
              if (offer)
                  $(this).find(".partnumber").text("MOTTAGARE");
              else    
                  $(this).find(".partnumber").text("PART " + idx);
              idx = idx + 1;
          }
          else {
              var text = "EJ UNDERTECKNANDE PART";
              if (offer) text = "AVSÄNDARE";
              $(this).find(".partnumber").text(text);
          }
      });
}

function updateCsvPersonIndex() {
  var input = $("form").find("input[type='hidden'][name='csvpersonindex']");
  var multipart = $("#personpane .multipart");
  if (multipart.length<1) {
    input.removeAttr("value");
  } else {
    var idx = multipart.parent().children().index(multipart);
    console.log("multipart is now " + idx);
    input.attr("value", idx);
  }
}

/*
 * When the author selects signatory, we have to 
 * remove the redborder for the validation error
 * where there is only zero signatories.
 */
safeReady(function() {
  var authorman = $(".authordetails .man");
  // where the red border appears for
  // addsig button
  var liplus = $("li.plus");
  $(".partyrole input", "#personpane").live("change", function() {
          if( $(this).attr("checked")) {
              authorman.removeClass('redborder');
              liplus.removeClass('redborder');
              renumberParts();
          }
      });
});

var signStepsContainer = null;
var signStepsWrapper = null;

safeReady(function() {
  signStepsContainer = $('#signStepsContainer');
  signStepsWrapper = $('#signStepsWrapper');
});

function resizeDesignBar() {
  if(signStepsContainer && signStepsWrapper) {
    if(!signStepsContainer.hasClass("fixed")) {
      signStepsWrapper.height(signStepsContainer.height());
    }
  }
}

safeReady(function() {
  $("#loadingdialog").overlay({
    mask: standardDialogMask,
    resizable: false,
//    onClose: function(e){ return false; },
    closeOnClick: false,
    closeOnEsc: false,
    load: false
  });
});

function displayLoadingOverlay(message) {
  $("#loadingmessage").html(message);
  $("#loadingdialog").overlay().load();
}

function closeLoadingOverlay() {
  $("#loadingdialog").overlay().close();
}

function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
	var c = ca[i];
	while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
	if (c.indexOf(nameEQ) == 0) {
      return c.substring(nameEQ.length);
    }
  }
  return null;
}

/** 
 * For Cross-Site Request Forgery (CSRF) Attacks
 * 1. Grab the cookie in Javascript, which protects
 *    against cross-domain attacks.
 * 2. Cram it into a form before it submits.
 * 3. This must be checked on the server.
 *
 * NOTE: This only protects againsts form submits. Links
 *   do not get protected by this code.
 */
safeReady(function() {
  $("form").live('submit', function() {
    var form = $(this);
    var tokenTag = $('<input type="hidden" name="xtoken">');
    var token = readCookie("xtoken");
    if(token && token.length > 0) {
      console.log(token);
      tokenTag.attr("value", token);
      form.append(tokenTag);
    }
  });
});
