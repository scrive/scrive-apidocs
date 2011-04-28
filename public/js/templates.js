// template field dragging system

// include this file to start the dragging functionality

// set this to true to output logging messages to the console
var debug = false;

// create a new signatory. pretty useless without calling signatoryToHTML
function newsignatory() {
  return {fstname: "",
          sndname: "", 
          company: "", 
          personalnumber: "", 
          companynumber: "", 
          email: "",
          signorder: 1,
	      nameplacements: [],
	      companyplacements: [],
	      personalnumberplacements: [],
          companynumberplacements: [],
	      emailplacements: [],
	      otherfields: []
         };
}

// a way to make drag live
(function ($) {
  jQuery.fn.liveDraggable = function (opts) {
    this.live("mouseover", function() {
      if (!$(this).data("draginit")) {
        $(this).data("draginit", true).draggable(opts);
      }
    });
  };
})(jQuery);

// a way to make drop live
// not used
(function ($) {
  jQuery.fn.liveDroppable = function (opts) {
    this.live("mouseover", function() {
      if (!$(this).data("dropinit")) {
        $(this).data("dropinit", true).droppable(opts);
      }
    });
  };
})(jQuery);

function newplacement(x, y, page, w, h) {
  return {x: x, y: y, page: page, w: w, h: h};
}

function placementToHTML(label, value) {
  var v = value;
  if(!v) {
    v = label;
  }
  var d = $("<div class='placedfield' style='cursor:pointer'><span class='value'></span></div>");
  $(".value",d).text(v);
  return d;
}

function placedFieldHelper(value) {
  var d = $("<div class='placedfieldhelper'><span class='value'></span></div>");
  $(".value", d).text(value);
  return d;
}

function placePlacements(pls, label, value, sigid, fieldid) {
  $(pls).each(function () {
    var pl = this;
    var d = placementToHTML(label, value);
    var page = $("#page" + pl.page);
    d.offset({left: pl.x, top: pl.y});
    page.append(d);
    pagewidth = $("input[name='width']",page).val();
    pageheight = $("input[name='height']",page).val();
    setSigID(d, sigid);
    setFieldID(d, fieldid);
    setHiddenField(d, "placedx", String(Math.round(pl.x)));
    setHiddenField(d, "placedy", String(Math.round(pl.y)));
    setHiddenField(d, "placedpage", String(pl.page));
    setHiddenField(d, "placedwidth", pagewidth);
    setHiddenField(d, "placedheight",  pageheight);
    d.css("position","absolute");
  });
}

function newHiddenValue(label, value) {
  return $("<span style='display: none'></span>").addClass(label).text(value);
}

// getHiddenValue does not care if it's hidden
function getHiddenValue(field, label) {
  var s = $(field).find("." + label);
  if(s.size()){
    return s.text();
  }
  if(debug) {
    console.log("field has no value named " + label);
  }
}

function setHiddenValue(field, label, value) {
  var s = $(field).find("." + label);
  if(s.size()){
    s.text(value);
  } else {
    field.append(newHiddenValue(label, value));
  }
}

function newHiddenField(name, value) {
  return $("<input type='hidden'/>").attr("value", value).attr("name", name);
}

// works on any input field, not just hidden ones
function getHiddenField(field, label) {
  var s = $(field).find("input[name='" + label + "']");
  if(s.size()){
    return s.attr("value");
  }
  if(debug) {
    console.log("field has no hidden field called " + label);
  }
}

function setHiddenField(field, label, value) {
  var s = $(field).find("input[name='" + label + "']");
  if(s.size()){
    s.attr("value", value);
  } else {
    field.append(newHiddenField(label, value));
  }
}

function magicUpdate(){
  var field = $(this).parents(".dragfield");
  var sigid = getSigID(field);
  var fieldid = getFieldID(field);
  var value = getValue(field);

  if(value === "") {
    value = getInfotext(field);
  }

  getPlacedFields().filter(function() {
    return getSigID(this) === sigid &&
      getFieldID(this) === fieldid;
  }).each(function() {
    setValue(this, value);
  });
  updateStatusForTyping(field);
}

function isDraggableField(field){
  return $(field).hasClass("dragfield") && !$(field).parents("sigentry").hasClass("dragDisabled");;
}

function isDraggableText(field){
    return $(field).hasClass("dragtext") && !$(field).parents("sigentry").hasClass("dragDisabled");
}

function isStandardField(field) {
  if(isDraggableField(field)) {
    var name = getFieldName(field);
    if(name == "signatoryemail"
       || name == "signatorysignorder"
       || name == "signatoryfstname"
       || name == "signatorysndname"
       || name == "signatorycompany"
       || name == "signatorypersonalnumber"
       || name == "signatorycompanynumber") {
      return true;
    }
  }
  return false;
}

function getValue(field) {
  if(isDraggableField(field)) {
    var s = $(field).find("input[type='text'], input[type='email']");
    if(s.size()) {
      if(s.attr("value") == s.attr("infotext")) {
	    return "";
      } else {
	    return s.attr("value");
      }
    } else {
      if(debug) {
        console.log("field has no input box");
      }
    }
  } else if(isDraggableText(field)) {
    return getHiddenValue(field, "fieldvalue");
  } else if(isPlacedField(field)) {
    return getHiddenValue(field, "value");
  } else if(debug) {
    console.log("I don't know what that field is");
  }
}

function setValue(field, value) {
  if(isDraggableField(field)){
    var s = $(field).find("input[type='text'], input[type='email']");
    if(s.size()) {
      s.focus();
      s.attr("value", value);
      s.change();
    } else {
      alert("field has no input box!");
    }
  } else if(isDraggableText(field)) {
    setHiddenValue(field, "fieldvalue", value);
  } else if(isPlacedField(field)) {
    setHiddenValue(field, "value", value);
  } else if(debug) {
    console.log("unknown type: " + getFieldType(field));
  }
}

function getFieldType(field) {
  return getHiddenValue(field, "type");
}

function setFieldType(field, type) {
  setHiddenValue(field, "type", type);
}

function getFieldID(field) {
  if(isDraggableField(field)) {
    if(isStandardField(field)) {
      return getHiddenValue(field, "fieldid");
    } else {
      return getHiddenField(field, "fieldid");
    }
  } else if(isDraggableText(field)) {
    return getHiddenValue(field, "fieldid");
  } else if(isPlacedField(field)) {
    return getHiddenField(field, "placedfieldid");
  } else if(debug) {
    console.log("unknown type");
  }
}

function setFieldID(field, fieldid) {
  if(isDraggableField(field)) {
    if(isStandardField(field)) {
      setHiddenValue(field, "fieldid", fieldid);
    } else {
      setHiddenField(field, "fieldid", fieldid);
    }
  } else if(isDraggableText(field)) {
    setHiddenValue(field, "fieldid", fieldid);
  } else if(isPlacedField(field)) {
    setHiddenField(field, "placedfieldid", fieldid);
  } else if(debug) {
    console.log("unknown type");
  }
}

function getFieldName(field) {
  var s = $(field).find("input[type='text'], input[type='email']");
  if(s.size()) {
    return s.attr("name");
  } else {
    alert("field has no text box");
  }
}

function setFieldName(field, name) {
  var s = $(field).find("input[type='text'], input[type='email']");
  if(s.size()) {
    s.attr("name", name);
  } else {
    alert("field has no text box");
  }
}

function getInfotext(field) {
  var s = $(field).find("input[type='text'], input[type='email']");
  if(s.size()) {
    return s.attr("infotext");
  } else {
    alert("field has no text box");
  }
}

function setInfotext(field, infotext) {
  var s = $(field).find("input[type='text'], input[type='email']");
  if(s.size()) {
    s.attr("infotext", infotext);
  } else {
    alert("field has no text box");
  }
}


function docstateToHTML(){
  var signatories = docstate.signatories;
  var authoridx = -1;
  docstate.author.signatory = false;
  for(i = 0; i < signatories.length; i++){
    if(signatories[i].email === docstate.author.email){
      authoridx = i;
      signatories[i].signatory = true;
      break;
    }
  }
  if(authoridx >= 0){
    docstate.author = signatories[authoridx];
    signatories.splice(authoridx, 1);
  }

  var sl = $("#personpane");
  

  authorToHTML(docstate.author);

  if(signatories.length === 0){
    signatories[0] = newsignatory();
  }
  
  csvpersonindex = sl.closest("form").find("input[type='hidden'][name='csvpersonindex']").attr("value");

  $(signatories).each(function (idx) {
    isMultiple = ("" + (idx+1)) === csvpersonindex 
    signatoryToHTML(isMultiple, this);
  });

  

  $("#personpane").children().each(function(idx) {
    var p = $(this);
    p.find(".partnumber").html("PART " + (idx + 1));
  });

  checkPersonPaneMode();

  renumberParts();

  $(".dragfield").each(function () {
    updateStatus(this);
  });
}

function getDragStatus(field){
  if(isDraggableField(field)){
    return getHiddenValue(field, "dragstatus");
  } else if(debug) {
    console.log(getFieldType(field) + " does not have an icon, cannot get");
  }
  return "";
}

function getFillStatus(field){
  if(isDraggableField(field)){
    return getHiddenValue(field, "fillstatus");
  } else if(debug) {
    console.log(getFieldType(field) + " does not have an icon, cannot get");
  }
  return "";
}

function setDragStatus(field, status) {
  if(isDraggableField(field)) {
    setHiddenValue(field, "dragstatus", status);
  } else if(debug) {
    console.log(getFieldType(field) + " does not have an icon, cannot set");
  }
}

function setFillStatus(field, status) {
  if(isDraggableField(field)) {
    setHiddenValue(field, "fillstatus", status);
  } else if(debug) {
    console.log(getFieldType(field) + " does not have an icon, cannot set");
  }
}

function getSigID(field) {
  if(isDraggableField(field)) {
    if(isStandardField(field)) {
      return getHiddenValue(field, "fieldsigid");
    } else {
      return getHiddenField(field, "fieldsigid");
    }
  } else if(isDraggableText(field)) {
    return getHiddenValue(field, "fieldsigid");
  } else if(isPlacedField(field)) {
    return getHiddenField(field, "placedsigid");
  } else if(debug) {
    console.log(getFieldType(field) + " does not have sigid");
  }
}

function setSigID(field, sigid) {
  if(isDraggableField(field)) {
    if(isStandardField(field)) {
      setHiddenValue(field, "fieldsigid", sigid);
    } else {
      setHiddenField(field, "fieldsigid", sigid);
    }
  } else if(isDraggableText(field)) {
    setHiddenValue(field, "fieldsigid", sigid);
  } else if(isPlacedField(field)) {
    setHiddenField(field, "placedsigid", sigid);
  } else if(debug) {
    console.log(getFieldType(field) + " does not have sigid");
  }
  
}

function getPlacedFieldsForField(field) {
  var fieldid = getFieldID(field);
  if(!fieldid) {
    console.log("not a field " + field);
    return $([]);
  } else {
    return getPlacedFields().filter(function() {
      return getFieldID(this) === fieldid;
    });
  }
}

function updateStatusForTyping(field) {
  field = $(field);
  var type = getFieldType(field);
  var dragstatus = getDragStatus(field);
  var oldfillstatus = getFillStatus(field);
  if(type == "author") {
    if(getValue(field)) {
      if(!(oldfillstatus === "done")) {
        setFillStatus(field, "done");
        if(oldfillstatus === "author" && fieldValidationType === "fillstatus" && field.hasClass("offending")) {
          field.removeClass('redborder');
        }
      }
    } else {
      if(!(oldfillstatus === "author")) {
        setFillStatus(field, "author");
        if(fieldValidationType === "fillstatus" && field.hasClass("offending")) {
          field.addClass('redborder');
        }
      }
    }
  } else if(type == "sig") {
    if(getValue(field)) {
      if(!(oldfillstatus === "done")) {
        setFillStatus(field, "done");
        if(fieldValidationType === "fillstatus") {
          field.removeClass('redborder');
        }
      }
    } else if(dragstatus === "placed") {
      if(!(oldfillstatus === "sig")) {
        setFillStatus(field, "sig");
      }
    } else if(isStandardField(field)) {
      setFillStatus(field, "done");
    } else {
      setFillStatus(field, "sig");
    }
  } else if(type == "text") {
    // do nothing
  } else if(debug) {
    console.log("field has bad field type: " + getFieldName(field));
  }
}

// function to automatically change status icon
// status is a function of field type and current state
function updateStatusForDragging(field) {
  invalidatePlacedFieldsCache();
  field = $(field);
  var olddragstatus = getDragStatus(field);
  var type = getFieldType(field);
  if(getPlacedFieldsForField(field).size()) {
    if(!(olddragstatus === "placed")){
      setDragStatus(field, "placed");
      if(fieldValidationType === "dragstatus" && olddragstatus === "must place" && field.hasClass("offending")) {
        field.removeClass('redborder');
      }
    }
  } else if(isStandardField(field)) {
    if(!(olddragstatus === "not placed")) {
      setDragStatus(field, "not placed");
      if(fieldValidationType === "dragstatus" && olddragstatus === "must place" && field.hasClass("offending")) {
        field.removeClass('redborder');
      }
    }
  } else {
    if(!(olddragstatus === "must place")) {
      setDragStatus(field, "must place");
      if(fieldValidationType === "dragstatus" && field.hasClass("offending")) {
        field.addClass("redborder");
      }
    }
  }
}

function updateStatus(field) {
  updateStatusForDragging(field);
  updateStatusForTyping(field);
}

function detachFieldsForSigID(sigid) {
  getPlacedFields().filter(function () {
    return sigid === getSigID(this);
  }).detach();
}

function detachFieldsForFieldID(fieldid) {
  getPlacedFields().filter(function () {
    return fieldid === getFieldID(this);
  }).detach();
}

function authorToHTML(sig) {
  var sl = $("#personpane");
  var sigid = "author";
  sig.id = sigid;
  
  var sigentry = sl.find(".authordetails");
  var manlink = sigentry.find("a.man");
  var sigcheck = sigentry.find(".partyrole input:radio").first();
  var seccheck = sigentry.find(".partyrole input:radio").last();

  if (sig.signatory) {
    sigcheck.attr("checked", "true");
  } else {
    seccheck.attr("checked", "false");
  }

  sigentry.find(".partyrole input:radio").first().change(function() {
    var checkBox = $(".sendcheckbox");
    if(checkBox.attr("checked")) { 
      checkBox.attr("checked", false).change();
    } 
  });

  manlink.click(function(){
    sigentry.find(".partyrole").show();
    manlink.addClass("selected");
    return false;
  });

  sigentry.find(".partyrole .closelink").click(function(){
    sigentry.find(".partyrole").hide();
    manlink.removeClass("selected");
    return false;
  });

  if(sig.signatory) {
    sigentry.find(".partyrole input:radio").first().attr("checked", "true");
  } else {
    sigentry.find(".partyrole input:radio").last().attr("checked", "true");
  }
  
  $("#peopleList ol").append(
      $("<li>").append(
          $("<a href='#'></a>").text(sig.fstname + " " + sig.sndname + " (Avsändare)").append(
              newSignOrderListElement(1)
          )
      )
  );
}

// BEGIN signing order related functions
function updatePeopleListSignOrder() {
    var signorderlist = $("#peopleList .signorderlist");
    $("#personpane .signorder").each(function(ix) {
        $(signorderlist.get(ix+1)).text($(this).find("option:selected").text());
    });
    updateAuthorSignOrder();
}

function updateAuthorSignOrder() {
    $("#peopleList .signorderlist:first").text(
        $("#authorsignorder").find("option:selected").text()
    );
}

function updateSignSendButton(authorsignorder) {
    // it can be 1, N (number of signatories + 1) or '-'.
    // '-' means author is not signatory, 1 that author
    // signs first and N that author signs last
    if (authorsignorder.val() != 1) {
        $(".sendbutton").show();
        $(".signbutton").hide();
    } else {
        $(".sendbutton").hide();
        $(".signbutton").show();
    }
}

function personIsSignatory(signorder) {
    return signorder.val() != "-";
}

function hideSigningOrderRelatedElements() {
    var authorsignorder = $("#authorsignorder");
    // we want it to switch to the first option if author is signatory,
    // however if there is no signatories, we will have two options with
    // value 1 and we want to switch to the first one, so we just change
    // it to 0 for now
    if (personIsSignatory(authorsignorder)) {
        authorsignorder.find("option:first").text(0);
        authorsignorder.val(0);
    }
    authorsignorder.hide();
    $(".signorder").hide();
    $(".signorderlist").hide();
    $("#signorderlisttitle").hide();
    updateSignSendButton(authorsignorder);
}

function showSigningOrderRelatedElements() {
    var authorsignorder = $("#authorsignorder");
    // restore proper value as we set it to 0 in hideSigning...
    authorsignorder.find("option:first").text(1);
    // author is signatory and we have at least one signatory beside the author
    if (personIsSignatory(authorsignorder) && !allNonSignatories)
        authorsignorder.show();
    $(".signorder").each(function() {
        var that = $(this);
        if (personIsSignatory(that))
            that.show();
    });
    $(".signorderlist").show();
    $("#signorderlisttitle").show();
    updateSignSendButton(authorsignorder);
}

function resetSignOrderValues(signorder) {
   // no signing order = everyone has signing order equal
   // to 1 so we just set every select value for that.
   signorder.val(1);
}

function removeLastSigningOrderPosition() {
    var authorsignorder = $("#authorsignorder"),
        authorsignorderlast = authorsignorder.find("option:last"),
        asolval = parseInt(authorsignorderlast.val());
    --asolval;
    authorsignorderlast.text(asolval);
    authorsignorderlast.val(asolval);
    updateAuthorSignOrder();
    
    if ($(".signorder:first option").length > 1) {
        $(".signorder").each(function() {
            var that = $(this),
                lo = that.find('option:last').remove(),
                losel = lo.attr("selected") != "";
            lo.remove();
            if (losel)
                that.val(that.find('option:last').val());
        });
    } else {
        authorsignorder.hide();
        allNonSignatories = true;
    }
}

function addSigningOrderPosition() {
    var authorsignorder = $("#authorsignorder"),
        authorsignorderlast = authorsignorder.find("option:last"),
        asolval = parseInt(authorsignorderlast.val());
     ++asolval;
     authorsignorderlast.text(asolval);
     authorsignorderlast.val(asolval);
     updateAuthorSignOrder();
    
    if (allNonSignatories) {
        // signing order is on and author is signatory
        if (signingOrderEnabled && personIsSignatory(authorsignorder))
            authorsignorder.show();
        allNonSignatories = false;
    } else {
        var signorder = $(".signorder"),
            // we want last, because first may contain only "-", and then
            // we get NaN. it's impossible for other selects to contain only
            // "-" since it's overwritten in signatoryToHTML if cloned select
            // contain only "-"
            signr = parseInt(signorder.last().find("option:last").val());
        ++signr;
        signorder.append(
            option = $("<option>").attr("value", signr).text(signr)
        );
    }
}

function newSignOrderListElement(value) {
    var signorderlist = $("<span class='signorderlist'>").text(value);
    if (!signingOrderEnabled)
        signorderlist.hide();
    return signorderlist;
}

safeReady(function() {
    // add all signatories to initial signorder template
    var signorder = $(".signorder"), siglen = docstate.signatories.length;
    console.log(siglen);
    if (siglen == 1)
        ++siglen; // empty doc has one sig (author)
    for (var i = 1; i < siglen; ++i)
        signorder.append($("<option>").attr("value", i).text(i));
    $("#authorsignorder option:last").val(siglen).text(siglen);
    
    if (signingOrderEnabled)
        showSigningOrderRelatedElements();
    else
        hideSigningOrderRelatedElements();
    
    $("#personpane #authorsignorder").change(function() {
        updateSignSendButton($(this));
        updatePeopleListSignOrder();
    });
    
    $("#personpane #authorsignatoryradio").change(function() {
        var authorsignorder = $("#authorsignorder"),
            option = authorsignorder.find("option:first");
        // restore firrs option's value
        option.text(1);
        option.val(1);
        authorsignorder.val(1);
        // signing order is enabled and we have another person that is signatory
        if (signingOrderEnabled && !allNonSignatories)
            authorsignorder.show();
        updateAuthorSignOrder();
        updateSignSendButton(authorsignorder);
    });
    
    $("#personpane #authorsecretaryradio").change(function() {
        var authorsignorder = $("#authorsignorder"),
            option = authorsignorder.find("option:first");
        if (signingOrderEnabled)
            authorsignorder.hide();
        // change first option to "-" and set it as active
        option.text("-");
        option.val("-");
        authorsignorder.val("-");
        updateAuthorSignOrder();
        updateSignSendButton(authorsignorder);
    });
    
    $("#personpane .signorder").live("change", updatePeopleListSignOrder);
    
    $("#personpane .sigrole_nonsignatory").live("change", function() {
        var signorder = $(this).parents(".persondetails").find(".signorder"),
            option = signorder.find("option:first");
        // change first option to "-" and set it as active
        option.text("-");
        option.val("-");
        signorder.val("-");
        signorder.hide();
        removeLastSigningOrderPosition();
        updatePeopleListSignOrder();
    });
    
    $("#personpane .sigrole_signatory").live("change", function() {
        var signorder = $(this).parents(".persondetails").find(".signorder"),
            option = signorder.find("option:first");
        // restore first option's value
        option.text(1);
        option.val(1);
        signorder.val(1);
        signorder.show();
        addSigningOrderPosition();
        updatePeopleListSignOrder();
    });
    
    $("#personpane .signingOrder").live("click", function() {
        if (signingOrderEnabled) {
            hideSigningOrderRelatedElements();
            $("#personpane .signorder").each(function() {
                resetSignOrderValues($(this));
            });
        } else {
            showSigningOrderRelatedElements();
            updatePeopleListSignOrder();
        }
        signingOrderEnabled = !signingOrderEnabled;
    });
});
// END signing order related functions

/*
 * Activate minus buttons within personpane.
 */

safeReady(function() {
  var signStepsWrapper = $('#signStepsWrapper');
  var signStepsContainer = $('#signStepsContainer');
  $('a.minus', $('#personpane')[0]).live('click', function() {
    var minus = $(this);

    { // this gets rid of custom fields and their placements
      var customfield = minus.parents('.customfield');
      getPlacedFieldsForField(customfield).detach();
      customfield.detach();
    }

    // we also need to adjust the size of the signStepsWrapper
    if(!signStepsContainer.hasClass('fixed')) {
      signStepsWrapper.height(signStepsContainer.height());
    }

    return false;
  });
});

/*
 * We need to protect against hitting the enter key in a 
 * field. Normally, that submits the form. We want it to do different
 * things depending on the field.
 */
safeReady(function() {
  // newfields should enter the name
  $('.newfield input', $('#personpane')[0]).live('keypress', function(e) {
    if(e.keyCode == 13) {
      var input = $(this);
      var newfield = input.parents('.newfield');
      createCustomField(newfield);
      return false;
    }
  });

  // all other inputs should not submit
  $('input', $('#personpane')[0]).live('keypress', function(e) {
    if(e.keyCode == 13) {
      return false;
    }
  });
});

/* 
 * We need to make the okIcon (ok button) createCustomField as well
 */
safeReady(function() { 
  $('.newfield a.okIcon', $('#personpane')[0]).live('click', function() {
    createCustomField($(this).parents('.newfield'));
    return false;
  });
  $('.newfield a.minusIcon', $('#personpane')[0]).live('click', function() {
    $(this).parents(".newfield").remove();
    // we also need to adjust the size of the signStepsWrapper
    if(!signStepsContainer.hasClass('fixed')) {
      signStepsWrapper.height(signStepsContainer.height());
    }

    return false;
  });
});

function createCustomField(newfield) {
  var thefield = newfield.find("input[type='text']");
  var fieldname = thefield.attr("value");
  var infotext = thefield.attr("infotext");
  if(fieldname === infotext || fieldname === "") {
    return false;
  }

  var customfield = $("#templates .customfield").first().clone();
  var input = customfield.find("input[type='text'], input[type='email']");
  input.attr("infotext", fieldname);
  input.val("");

  var persondetails = newfield.parents(".persondetails");
  //  console.log(persondetails);
  if(persondetails.hasClass('authordetails')) {
    setFieldType(customfield, 'author');
  } else {
    setFieldType(customfield, "sig");
  }


  setFieldName(customfield, "fieldvalue");
  var fieldid = newUUID();
  var sigid = "";
  if(persondetails.hasClass('authordetails')) {
    sigid = "author";
  } else {
    sigid = getHiddenField(persondetails, 'sigid');
  }

  setFieldID(customfield, fieldid);
  setSigID(customfield, sigid);
  setHiddenField(customfield, "fieldname", fieldname);

  newfield.replaceWith(customfield);

  enableInfoTextOnce(customfield);
  updateStatus(customfield);
  
  customfield.find(".customfieldbox").focus();
  return false;
}

/*
 * All fields in #personpane are editable and potentially have
 * placedfields. magicUpdate takes care of changing all of the
 * placedfields.
 */
safeReady(function() {
  $('.dragfield input', $('#personpane')[0]).live('keyup keydown change', magicUpdate);
});

/*
 * The plus button exists in all .persondetails boxes
 * It always needs to add a newfield
 */
safeReady(function() {
  var signStepsWrapper = $('#signStepsWrapper');
  var signStepsContainer = $('#signStepsContainer');
  $('a.plus', $('#personpane')[0]).live('click', function() {
    var plus = $(this);
    var persondetails = plus.parents('.persondetails');
    var otherfields = persondetails.find('.otherfields');
    var newfield = $("<div class='newfield inputWrapper'><input class='newfieldbox' type='text' infotext='Namnge fältet' /><a href='#' class='minusIcon icon small remove'></a><a href='#' class='okIcon icon small ok'></a></div>");

    otherfields.append(newfield);
    enableInfoTextOnce(newfield);
    // we also need to adjust the size of the signStepsWrapper
    if(!signStepsContainer.hasClass('fixed')) {
      signStepsWrapper.height(signStepsContainer.height());
    }
    return false;
  });
});

safeReady(function() {
  $('.nextproblem').live('click', function() {
    var div = $(this).closest(".csvproblemcontainer");
    div.removeClass("currentcsvproblem");
    div.next().addClass("currentcsvproblem");
  });
});

safeReady(function() {
  $('.previousproblem').live('click', function() {
    var div = $(this).closest(".csvproblemcontainer");
    div.removeClass("currentcsvproblem");
    div.prev().addClass("currentcsvproblem");
  });
});

safeReady(function() {
  $('.nextcsvpage').live('click', function() {
    var div = $(this).closest(".csvpagecontainer");
    div.removeClass("currentcsvpage");
    div.next().addClass("currentcsvpage");
  });
});

safeReady(function() {
  $('.previouscsvpage').live('click', function() {
    var div = $(this).closest(".csvpagecontainer");
    div.removeClass("currentcsvpage");
    div.prev().addClass("currentcsvpage");
  });
});

function setUpCSVUpload(sigentry) {
  var form = sigentry.closest("form");
  var div = sigentry.closest("#personpane").find(".currentPerson");
  var idx = div.parent().children().index(div);
  form.find("input[type='hidden'][name='csvpersonindex']").removeAttr("value");
  form.find("input[type='hidden'][name='csvpersonindex']").attr("value", idx);
}

function tearDownCSVUpload(sigentry) {
  var form = sigentry.closest("form");
  form.find("input[type='hidden'][name='csvpersonindex']").attr("value", "");
}

function setupAsMultiplePart(sigentry) {
  sigentry.addClass("multipart");
  var icons = sigentry.find('.signStepsBodyIcons');
  icons.find(".csv.single").hide();
  icons.find(".csv.multi").show();
}

function setupAsSinglePart(sigentry) {
  sigentry.removeClass("multipart");
  var icons = sigentry.find('.signStepsBodyIcons');
  icons.find(".csv.single").show();
  icons.find(".csv.multi").hide();
  sigentry.find("input[name='signatoryfstname']").change();
}

function signatoryToHTML(isMultiple, sig) {
  var sl = $("#personpane");
  var sigid = newUUID();
  sig.id = sigid;

  var sigentry = $('#templates .persondetails').first().clone();

  setHiddenField(sigentry, "sigid", sigid);
  
  if (isMultiple) {
    setupAsMultiplePart(sigentry);
  } else {
    setupAsSinglePart(sigentry);
  }

  var manlink = sigentry.find("a.man");

  manlink.click(function(){
    sigentry.find(".partyrole").show();
    manlink.addClass("selected");
    return false;
  });

  sigentry.find(".partyrole .closelink").click(function(){
    sigentry.find(".partyrole").hide();
    manlink.removeClass("selected");
    return false;
  });

  var radiobuttons = sigentry.find(".partyrole input:radio");
  radiobuttons.change(function() {
    if( $(this).attr("checked")) {
      sigentry.find(".partyrole input:hidden").val($(this).attr("value"));
      var that = this;
      radiobuttons.each( function() {
        if( that!=this ) {
          $(this).attr("checked",false);
        }
      });
    }
  });


  var d = sigentry.find(".fields");
  var of = sigentry.find(".otherfields");
  
  var afstname = sigentry.find(".sigfstname");
  var asndname = sigentry.find(".sigsndname");
  var acomp = sigentry.find(".sigcomp");
  var apersnumb = sigentry.find(".sigpersnum");
  var acompnumb = sigentry.find(".sigcompnum");
  var aemai = sigentry.find(".sigemail");

  setSigID(afstname, sigid);
  setSigID(asndname, sigid);
  setSigID(acomp, sigid);
  setSigID(apersnumb, sigid);
  setSigID(acompnumb, sigid);
  setSigID(aemai, sigid);

  setValue(afstname, sig.fstname);
  setValue(asndname, sig.sndname);
  setValue(acomp, sig.company);
  setValue(apersnumb, sig.personalnumber);
  setValue(acompnumb, sig.companynumber);
  setValue(aemai, sig.email);
  // other fields
  
  $(sig.otherfields).each(function (){
    var fd = this;
    var field = $("#templates .customfield").first().clone();
    var fieldid = newUUID();
    fd.id = fieldid;
    
    setFieldID(field, fieldid);
    setSigID(field, sigid);
    setFieldName(field, "fieldvalue");
    setHiddenField(field, "fieldname", fd.label);

    setInfotext(field, fd.label);
    setValue(field, fd.value);
    setFieldType(field, "sig");

    var input = field.find("input");

    of.append(field);
  });

  enableInfoTextOnce(sigentry);
  
  var n = "Unnamed";

  if (isMultiple) {
    n = "Massutskick"
  } else if(sig.fstname == "" && sig.sndname == "") {
    n = "(Namnlös)";
  } else {
    n = sig.fstname + " " + sig.sndname;
  }
  
  if (sig.signorder > 1) {
      signingOrderEnabled = true;
      showSigningOrderRelatedElements();
  }
  
  var sigs = $("#peopleList ol");
  sigs.append(
      $("<li>").append(
          $("<a href='#'></a>").text(n).append(
              newSignOrderListElement(sig.signorder)
          )
      )
  );
  sl.append(sigentry);
  
  // set sign order
  var signorder = $("#personpane .signorder:eq(" + (sigs.find("li").length-2) + ")");
  signorder.val(sig.signorder);
  
  sigentry.find(".csv.single").overlay({
    mask: standardDialogMask,
    onBeforeLoad: function() {
      csvpersonindex = $("form").find("input[type='hidden'][name='csvpersonindex']").attr("value");
      if (!(csvpersonindex && csvpersonindex.length>0)) {
        setUpCSVUpload(sigentry);
        return true;
      } else {
        return false;
      }
    },
    onClose: function() {
      tearDownCSVUpload(sigentry);
      return true;
    }
  });

  sigentry.find(".csv.multi").overlay({
    mask: standardDialogMask
  });
}

function placePlacementsOfSignatories(signatories) {
  $(signatories).each(function(){
    var sig = this;
    placePlacements(sig.fstnameplacements, "Förnamn", sig.fstname, sig.id, "fstname");
    placePlacements(sig.sndnameplacements, "Efternamn", sig.sndname, sig.id, "sndname");
    placePlacements(sig.companyplacements, "Företag", sig.company, sig.id, "company");
    placePlacements(sig.personalnumberplacements, "Persnr", sig.personalnumber, sig.id, "personalnumber");
    placePlacements(sig.companynumberplacements, "Orgnr", sig.companynumber, sig.id, "companynumber");
    placePlacements(sig.emailplacements, "Personens e-mail", sig.email, sig.id, "email");
    $(sig.otherfields).each(function(){
      var fd = this;
      placePlacements(fd.placements, fd.label, fd.value, sig.id, fd.id);
    });
  });
}

function getLabel(x) {
  var label = $(x).find("input").attr("infotext");
  if(!label) {
    label = "nolabel";
  }
  return label;
}

function isPlacedField(field) {
  return $(field).hasClass("placedfield");
}

function initializeTemplates () {
  var pagediv = $(".pagediv");
  if(pagediv.size() === 0){
    setTimeout("initializeTemplates();", 100);
    return;
  }

  $(".pagediv").droppable({ drop: function(event, ui) {
    var page = $(this);
    var field = $(ui.draggable);
    var helper = $(ui.helper);
    
    var top = helper.offset().top - page.offset().top;
    var left = helper.offset().left - page.offset().left;
    
    var pageno = parseInt(page.attr("id").substr(4));
    
    var sigid = getSigID(field);
    var fieldid = getFieldID(field);
    var pl = newplacement(left, top, pageno, page.width(), page.height());
    placePlacements([pl], getLabel(field), getValue(field), sigid, fieldid);
    
    if(isPlacedField(field)) {
      field.detach();
      helper.detach();
    }
    hideCoordinateAxes();
    updateStatus(field);
    return false;
  }});

  
  // coordinate axes
  var pagejpg = pagediv.find(".pagejpg");
  pagejpg.append("<div class='hline'></div>");
  pagejpg.append("<div class='vline'></div>");
  
  placePlacementsOfSignatories(docstate.signatories);
  placePlacementsOfSignatories([docstate.author]);
  enableInfoTextOnce();
  
  $(".dragfield, .dragtext").each(function() {
    updateStatus($(this));
  });

  var people = $("#personpane").children().removeClass("currentPerson");
  if (initialperson<1 || initialperson>people.length) {
    people.last().addClass("currentPerson");
  } else {
    people.eq(initialperson - 1).addClass("currentPerson");
    if (isaftercsvupload) {
      people.eq(initialperson - 1).find(".csv.multi").click();
    }
  }

  $("form").submit(function () {
    var form = $("form");
    $(".placedfield input[type='hidden']").each(function () {
      var h = $(this);
      form.append(h);
    });
  });
}

safeReady(function () {
  docstateToHTML();
  initializeTemplates();	
});

// utility functions

function newUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
  }).toUpperCase();
}

// this stuff keeps the editing fast

var placedfieldscache = null;

function getPlacedFields() {
  if(placedfieldscache === null) {
    placedfieldscache = $(".placedfield");
  }
  return placedfieldscache;
}

function invalidatePlacedFieldsCache() {
  placedfieldscache = null;
}

function moveCoordinateAxes(helper) {
  var hline = $(".hline");
  var vline = $(".vline");
  hline.each(function() {
    var h = $(this);
    var page = h.parents(".pagejpg");
    h.css({
      top: Math.min(page.height() - 1, 
                    Math.max(0, 
                             helper.offset().top - page.offset().top + helper.height() - 4)) 
        + "px"
    });
  });
  vline.each(function() {
    var v = $(this);
    var page = v.parents(".pagejpg");
    v.css({
      left: Math.min(page.width() - 1, 
                     Math.max(0, 
                              helper.offset().left - page.offset().left)) 
        + "px"
    });
  });
}

function showCoordinateAxes(helper) {
  var hline = $(".hline");
  var vline = $(".vline");
  hline.show();
  vline.show();
  $("body").mousemove(function() { setTimeout(function() { moveCoordinateAxes(helper); },
                                              100); });
}

function hideCoordinateAxes() {
  $(".hline").hide();
  $(".vline").hide();
  // we need to unbind it so actions won't keep stacking
  $("body").unbind("mousemove");
}

safeReady(function() {

  $(".dragfield", "#personpane")
    .liveDraggable({ handle: ".draghandle",

		             appendTo: "body",
		             helper: function(event) {
		               var field = $(this);
		               var input = field.find("input");
		               return placedFieldHelper(input.attr("value"));
		             },
		             start: function(event, ui) {
                       showCoordinateAxes(ui.helper);
                     },
                     stop: function() {
                       hideCoordinateAxes();
                     },
                     drag: function(event, ui) {
                       moveCoordinateAxes(ui.helper);
                     }
	               });

  $(".dragtext", "#personpane")
    .liveDraggable({ handle: ".draghandle",

		             appendTo: "body",
		             helper: function() {
		               return placedFieldHelper($(this).find(".fieldvalue").text());
	                 },
	                 start: function(event, ui) {
                       showCoordinateAxes(ui.helper);
                     },
                     stop: function() {
                       hideCoordinateAxes();
                     },
                     drag: function(event, ui) {
                       moveCoordinateAxes(ui.helper);
                     }
		           });

  $(".placedfield", "#documentBox")
    .liveDraggable({
      appendTo: "body",
      stop: function(event, ui) {
        var field = $(event.target);
        var helper = $(ui.helper);
        
        hideCoordinateAxes();
        if(isPlacedField(field)) {
          helper.detach();
          field.detach();
        }
        
        var fieldid = getFieldID(field);
        $('.dragfield').filter(function() {
          return getFieldID(this) === fieldid;
        }).each(function() {
          updateStatus(this);
        });
      },
      // build a helper so it doesn't delete the original
      helper: function(event, ui) {
        return placedFieldHelper($(this).find(".value").text());
      },
      // but we don't want to show the original so it looks like 
      // you are dragging the original
      start: function(event, ui) {
        var that = $(this);
        that.hide();
        showCoordinateAxes(ui.helper);
      },
      drag: function(event, ui) {
        moveCoordinateAxes(ui.helper);
      }
    });
});
