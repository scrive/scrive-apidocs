// template field dragging system

// include this file to start the dragging functionality

// set this to true to output logging messages to the console
var debug = false;

// create a new signatory. pretty useless without calling signatoryToHTML
function newsignatory() {
  return {name: "", company: "", number: "", email: "", 
	  nameplacements: [],
	  companyplacements: [],
	  numberplacements: [],
	  emailplacements: [],
	  otherfields: []};
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
  return $("<div class='placedfield' style='position:absolute'>"
	   + "<span class='value'>" + v + "</span>" 
	   + "</div>");
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
    setHiddenField(d, "placedx", String(Math.round(d.position().left)));
    setHiddenField(d, "placedy", String(Math.round(d.position().top)));
    setHiddenField(d, "placedpage", String(pl.page));
    setHiddenField(d, "placedwidth", pagewidth);
    setHiddenField(d, "placedheight",  pageheight);
  });
}

function newHiddenValue(label, value) {
  return $("<span style='display: none' class='" + label + "'>" + value + "</span>");
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
  return $("<input type='hidden' name='" + name + "' value='" + value + "' />");
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
    field.append("<input type='hidden' name='" + label + "' value='" + value + "' />");
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
  return $(field).hasClass("dragfield");
}

function isDraggableText(field){
  return $(field).hasClass("dragtext");
}

function isStandardField(field) {
  if(isDraggableField(field)) {
    var name = getFieldName(field);
    if(name == "signatoryemail" 
       || name == "signatoryname"
       || name == "signatorycompany"
       || name == "signatorynumber") {
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
function setTitle(field,value)
{
  
  if (value.length > 12)
    $(field).attr("title",value);
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

function buildDraggableField(info, val, type, emailp) {
  var x = $("<div class='dragfield'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span><input type='text' autocomplete='off' /><span class='dragstatus'></span> <span class='fillstatus'></span></div>");
  if(emailp) {
    x = $("<div class='dragfield'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span><input type='email' autocomplete='off' /><span class='dragstatus'></span> <span class='fillstatus'></span></div>");
  }


  setInfotext(x, info);
  setValue(x, val);
  setFieldType(x, type);
/*
  x.draggable({ handle: ".draghandle"
		, zIndex: 10000
		, appendTo: "body"
		, helper: function (event) {
		  var field = $(this);
		  var input = field.find("input");
		  return placementToHTML(input.attr("infotext"), input.attr("value"));
	        }
	      });
              */
  var input = x.find("input");
  return x;
}

// for use with author fields
function buildDraggableText(val) {
  var x = $("<div class='dragtext'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span> <span class='fieldvalue'>" + val + "</span></div>");

/*
  x.draggable({ handle: ".draghandle"
		, zIndex: 10000
		, appendTo: "body"
		, helper: function (event) {
		  return placementToHTML("abc", val);
	        }
	      });
              */
  return x;
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
  
  $(signatories).each(function () {
    signatoryToHTML(this);
  });

  

  $("#personpane").children().each(function(idx) {
    var p = $(this);
    p.find(".partnumber").html("PART " + (idx + 1));
  });

  checkPersonPaneMode();

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
        if(fieldValidationType === "fillstatus" && field.hasClass("offending")) {
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
  //  setHiddenField(sigentry, "sigid", sigid);
  var manlink = sigentry.find("a.man");

  manlink.click(function(){
    sigentry.find(".partyrole").show();
    if(sigentry.find(".partyrole input[checked]").size() === 0){
      sigentry.find(".partyrole input").first().attr("checked", "true");
    }
    return false;
  });

  sigentry.find(".partyrole .closelink").click(function(){
    sigentry.find(".partyrole").hide();
    return false;
  })

  if(sig.signatory) {
    sigentry.find(".partyrole input").first().attr("checked", "true");
  } else {
    sigentry.find(".partyrole input").last().attr("checked", "true");
  }

/*
  sigentry.find(".dragtext").draggable({ handle: ".draghandle"
		                         , zIndex: 10000
		                         , appendTo: "body"
		                         , helper: function () {
		                           return placementToHTML(getValue(this));
	                                 }
		                       });
                                       */
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
    setFieldType(field, "author");
/*
    field.draggable({ handle: ".draghandle"
		      , zIndex: 10000
		      , appendTo: "body"
		      , helper: function (event) {
			var field = $(this);
			var input = field.find("input");
			return placementToHTML(input.attr("infotext"), input.attr("value"));
		      }
		    });
                    */
    sigentry.find(".otherfields").append(field);
    //	    
  });

  $("#peopleList ol").append("<li><a href='#'>" + sig.name + " (Avsändare)</a></li>");
}

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

/*
  customfield.draggable({ handle: ".draghandle",
                          zIndex: 10000,
		          appendTo: "body",
		          helper: function (event) {
		            var field = $(this);
		            var input = field.find("input");
		            return placementToHTML(input.attr("infotext"), input.attr("value"));
		          }
	                });
                        */
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
    var newfield = $("<div class='newfield inputWrapper'><input class='newfieldbox' type='text' infotext='Namnge fältet' /><a href='#' class='okIcon'>OK</a></div>");

    otherfields.append(newfield);
    enableInfoTextOnce(newfield);
    // we also need to adjust the size of the signStepsWrapper
    if(!signStepsContainer.hasClass('fixed')) {
      signStepsWrapper.height(signStepsContainer.height());
    }
    return false;
  });
});

function signatoryToHTML(sig) {
  var sl = $("#personpane");
  var sigid = newUUID();
  sig.id = sigid;

  var sigentry = $('#templates .persondetails').first().clone();


  //var sigentry = $("<div class='sigentry'></div>");
  //sigentry.append(newHiddenField("sigid", sigid));
  setHiddenField(sigentry, "sigid", sigid);

  var d = sigentry.find(".fields");
  var of = sigentry.find(".otherfields");
  
  var aname = sigentry.find(".signame");
  var acomp = sigentry.find(".sigcomp");
  var anumb = sigentry.find(".signum");
  var aemai = sigentry.find(".sigemail");
  

  //setSigID(sigentry, sigid);

  setSigID(aname, sigid);
  setSigID(acomp, sigid);
  setSigID(anumb, sigid);
  setSigID(aemai, sigid);

  setValue(aname, sig.name);
  setValue(acomp, sig.company);
  setValue(anumb, sig.number);
  setValue(aemai, sig.email);
/*
  sigentry.find(".dragfield").draggable({ handle: ".draghandle"
		                          , zIndex: 10000
		                          , appendTo: "body"
		                          , helper: function (event) {
		                            var field = $(this);
		                            var input = field.find("input");
		                            
		                            return placementToHTML(input.attr("infotext"), input.attr("value"));
	                                  }
		                        });
                                        */

  /*
    aemai.find("input").attr('id', 'othersignatoryemail');
    if(aemai.find("input").attr("value")) {
    setIcon(aemai, "done");
    }
  */
  
  // other fields
  
  $(sig.otherfields).each(function (){
    var fd = this;
    var field = $("#templates .customfield").first().clone();
    //var field = buildDraggableField(fd.label, fd.value, "sig");
    var fieldid = newUUID();
    fd.id = fieldid;
    
    setFieldID(field, fieldid);
    setSigID(field, sigid);
    setFieldName(field, "fieldvalue");
    setHiddenField(field, "fieldname", fd.label);

    setInfotext(field, fd.label);
    setValue(field, fd.value);
    setFieldType(field, "sig");
/*
    field.draggable({ handle: ".draghandle"
		      , zIndex: 10000
		      , appendTo: "body"
		      , helper: function (event) {
			var field = $(this);
			var input = field.find("input");
			return placementToHTML(input.attr("infotext"), input.attr("value"));
		      }
		    });
                    */
    var input = field.find("input");

    of.append(field);
    //	    
  });

  enableInfoTextOnce(sigentry);
  
  var n = "Unnamed";

  if(sig.name == "") {
    n = "(Namnlös)";
  } else {
    n = sig.name;
  }
  
  $("#peopleList ol").append("<li><a href='#'>" + n + "</a></li>");
  sl.append(sigentry);

}

function placePlacementsOfSignatories(signatories) {
  $(signatories).each(function(){
    var sig = this;
    placePlacements(sig.nameplacements, "Namn på motpart", sig.name, sig.id, "name");
    placePlacements(sig.companyplacements, "Titel, företag", sig.company, sig.id, "company");
    placePlacements(sig.numberplacements, "Orgnr/Persnr", sig.number, sig.id, "number");
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

safeReady(function() {
  $("#signStepsContainer").droppable({ drop: function(event, ui) {
    var field = $(ui.draggable);
    var helper = $(ui.helper);
    var fieldid = getFieldID(field);
    if(isPlacedField(field)) {
      field.detach();
      helper.detach();  

      $('.dragfield').filter(function() {
        return getFieldID(this) === fieldid;
      }).each(function() {
        updateStatus(this);
      });
    }
    return false;
  }});
});

safeReady(function() {
  $(".pagediv", "#documentBox")
    .liveDroppable({ drop: function(event, ui) {
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
      updateStatus(field);
      return false;
    }});
  
  return true;
});

function initializeTemplates () {
  if($(".pagediv").size() === 0){
    setTimeout("initializeTemplates();", 100);
    return;
  }

  placePlacementsOfSignatories(docstate.signatories);
  placePlacementsOfSignatories([docstate.author]);
  enableInfoTextOnce();
  
//  makeDropTargets();

  $(".dragfield, .dragtext").each(function() {
    updateStatus($(this));
  });

  $("#personpane").children().removeClass("currentPerson").last().addClass("currentPerson");

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

safeReady(function() {
  $(".dragfield", "#personpane")
    .liveDraggable({ handle: ".draghandle",
                     zIndex: 10000,
		     appendTo: "body",
		     helper: function (event) {
		       var field = $(this);
		       var input = field.find("input");
		       return placementToHTML(input.attr("infotext"), input.attr("value"));
		     }
	           });
  $(".dragtext", "#personpane")
    .liveDraggable({ handle: ".draghandle",
		     zIndex: 10000,
		     appendTo: "body",
		     helper: function () {
		       return placementToHTML($(this).find(".fieldvalue").html());
	             }
		   });

  $(".placedfield", "#documentBox")
    .liveDraggable({ 
      appendTo: "body",
      stop: function(event, ui) {
        var field = $(event.target);
        var helper = $(ui.helper);
        
        // for some reason, this needs to be helper (field is
        // empty)
        if(isPlacedField(helper)) {
          $(this).detach();
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
      helper: function (event, ui) {
        return placementToHTML($(this).find(".value").html());
      },
      // but we don't want to show the original so it looks like 
      // you are dragging the original
      start: function (event, ui) {
	$(this).hide();
      }
    });
});
