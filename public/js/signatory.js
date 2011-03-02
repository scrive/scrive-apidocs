

var debug = false;

function newUUID() {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
	    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
	    return v.toString(16);
	}).toUpperCase();
}

function newsignatory() {
    return {fstname: "", sndname:"", company: "", number: "", email: "", 
	    fstnameplacements: [],
            sndnameplacements: [],
            companyplacements: [],
	    numberplacements: [],
	    emailplacements: [],
	    otherfields: []};
}

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
	    
	    setSigID(d, sigid);
	    setFieldID(d, fieldid);
	});
}

function newHiddenValue(label, value) {
    return $("<span style='display: none' class='" + label + "'>" + value + "</span>");
}

function getHiddenValue(field, label) {
    var s = $(field).find("." + label);
    if(s.size()){
	return s.text();
    } else if(debug) {
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

function getHiddenField(field, label) {
    var s = $(field).find("input[name='" + label + "']");
    if(s.size()){
	return s.attr("value");
    } else if(debug) {
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

    if(value == "") {
	value = getInfotext(field);
    }

    $(".placedfield").each(function () {
	    var f = $(this);
	    if(getSigID(f) == sigid
	       && getFieldID(f) == fieldid) {
		setValue(f, value);
	    }
	});
    updateStatus(field);
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
	   || name == "signatoryfstname"
           || name == "signatorysndname"
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
	} else if(debug) {
	    console.log("field has no input box");
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
	    s.attr("value", value);
	} else {
	    alert("field has no input box!");
	}
    } else if(isDraggableText(field)) {
	setHiddenValue(field, "value", value);
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

function buildField(info, val, type) {
    var x = $("<div class='dragfield'><input type='text' name='fieldvalue' autocomplete='off' /></div>");

    setInfotext(x, info);
    setValue(x, val);
    setFieldType(x, type);

    var input = x.find("input");

    input.keydown(magicUpdate);
    input.keyup(magicUpdate);
    input.change(magicUpdate);
    return x;
}

function docstateToHTML(){
    var useremail = docstate['useremail'];
    /*
    var author = docstate.author;

    placePlacements(author.nameplacements, "Namn", author.name, "author", "name");
    placePlacements(author.emailplacements, "email", author.email, "author", "email");
    placePlacements(author.companyplacements, "company", author.company, "author", "sigco");
    placePlacements(author.numberplacements, "number", author.number, "author", "signr");

    */
    var currentsig;
    var currentsigdiv = $("<span />");

    $(".signViewBodyRight").each(function() {
	    var f = $(this);
	    if(f.text().indexOf(useremail) > -1) { // hacky; TODO add actual divs with classnames
		currentsigdiv = f;
	    }
	});

    var fields = currentsigdiv.find(".signViewBodyForms");


    $(docstate.signatories).each(function() {
	    var s = this;
	    var cc = false;
	    s.id = newUUID();
	    if(s.email === useremail) {
		currentsig = s;
		cc = true;
	    }

	    placePlacements(this.fstnameplacements, "Förnamn", this.fstname, s.id, "fstname");
            placePlacements(this.sndnameplacements, "Efternamn", this.sndname, s.id, "sndname");
	    placePlacements(this.emailplacements, "Personens e-mail", this.email, s.id, "email");
	    placePlacements(this.companyplacements, "Titel, företag", this.company, s.id, "sigco");
	    placePlacements(this.numberplacements, "Orgnr/Persnr", this.number, s.id, "signr");

	    

	    $(s.otherfields).each(function () {
		    var f = this;
		    f.id = newUUID();
		    placePlacements(f.placements, f.label, f.value, s.id, f.id);
		    if(!cc){
		    $(".signViewBodyRight").each(function() {
			    var ff = $(this);
			    //console.log(s);
			    if(ff.text().indexOf(s.email) > -1) {
                              var val = f.value;
                              if(val === ""){
                                val = "(unfilled)";
                              }
			      ff.find(".signatoryfields").append("<span class='text'>" + f.label + ": " +val+"</span>");
			    }
			});
		    }
		});
	});

    if(currentsig && currentsig.company.length === 0 && currentsig.companyplacements.length > 0) {
	var cfield = buildField("Titel, företag", currentsig.company, "sig");
	setFieldID(cfield, "sigco");
	setSigID(cfield, currentsig.id);
	setHiddenField(cfield, "fieldname", "sigco");
	fields.append(cfield);
	updateStatus(cfield);
	enableInfoTextOnce(cfield);
    }

    if(currentsig && currentsig.number.length === 0 && currentsig.numberplacements.length > 0) {
	var nfield = buildField("Orgnr/Persnr", currentsig.number, "sig");
	setFieldID(nfield, "signr");
	setSigID(nfield, currentsig.id);
	setHiddenField(nfield, "fieldname", "signr");
	fields.append(nfield);
	updateStatus(nfield);
	enableInfoTextOnce(nfield);
    }

    $(currentsig && currentsig.otherfields).each(function () {
	    var f = this;
	    if(f.value.length === 0){
		ofield = buildField(f.label, f.value, "sig");
		setFieldID(ofield, f.id);
		setSigID(ofield, currentsig.id);
		setHiddenField(ofield, "fieldname", f.label);
		fields.append(ofield);
		updateStatus(ofield);
		enableInfoTextOnce(ofield);
	    } else {
	      $(".signViewBodyRight").each(function() {
		var ff = $(this);
		//console.log(s);
		if(ff.text().indexOf(currentsig.email) > -1) {
		  ff.find(".signatoryfields").append("<span class='text'>" + f.label + ": " +f.value+"</span>");
		}
	      });

            }
	});
}

function getIcon(field){
    if(isDraggableField(field)){
	return getHiddenValue(field, "status");
    } else if(debug) {
	console.log(getFieldType(field) + " does not have an icon, cannot get");
    }
    return "";
}

function setIcon(field, status) {
    if(isDraggableField(field)) {
	setHiddenValue(field, "status", status);
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
    var fields = [];
    var fieldsigid = getSigID(field);
    $(".placedfield").each(function() {
	    var p = $(this);
	    var fid = getFieldID(p);
	    var sid = getSigID(p);
	    if(fid == fieldid) {
		fields[fields.length] = p;
	    }
	});
    return $(fields);
}

// function to automatically change status icon
// status is a function of field type and current state
function updateStatus(field) {
  field = $(field);
  var type = getFieldType(field);
  if(type == "author") {
    if(getPlacedFieldsForField(field).size()){
      setDragStatus(field, "placed");
      //field.removeClass('redborder');
    } else if (isStandardField(field)) {
      setDragStatus(field, "not placed");
    } else {
      setDragStatus(field, "must place");
    }
    if(getValue(field)) {
      setFillStatus(field, "filled");
      field.removeClass('redborder');
    } else {
      setFillStatus(field, "author");
    }
  } else if(type == "sig") {
    if(getPlacedFieldsForField(field).size()){
      setDragStatus(field, "placed");
      field.removeClass('redborder');
    } else if(isStandardField(field)) {
      setDragStatus(field, "not placed");
    } else {
      setDragStatus(field, "must place");
    }
    if(getValue(field)) {
      // it's got a value
      setFillStatus(field, "done");
      field.removeClass('redborder');
    } else if(getPlacedFieldsForField(field).size()) {
      // not filled out, but it's placed
      setFillStatus(field, "sig");
      field.removeClass('redborder');
    } else {
      // not filled out, not placed, so it's handled above
      setFillStatus(field, "sig");
    }
  } else if(type == "text") {
    // do nothing
  } else if(debug) {
    console.log("field has bad field type: " + getFieldName(field));
  }
}

function detachFieldsForSig(sigid) {
    $(".placedfield").each(function () {
	    var field = $(this);
	    if(sigid == getSigID(field)){
		field.detach();
	    }
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

function initializeTemplates () 
{
    if($(".pagediv").size() == 0){
        setTimeout(function() { initializeTemplates();}, 100);
        return;
    }

    $("#loading-message").css({ display: "none" });
    $("#edit-bar").css({ display: "" });

    docstateToHTML();
	
    $("form#dialog-confirm-sign").submit(function(){
	    var form = $(this);
	    $(".dragfield").each(function(){
		    var field = $(this);
		    form.append("<input type='hidden' name='fieldvalue' value='" + getValue(field) + "' />");		    
		    form.append("<input type='hidden' name='fieldname' value='" + getHiddenField(field, "fieldname") + "' />");
		});
	});

    $("#sign").show();
    $("#cancel").show();
}

$(document).ready(function () {
	$("#loading-message").css({ display: "" });
	$("#edit-bar").css({ display: "none" });

	$("#sign").hide();
	$("#cancel").hide();

	
	
	initializeTemplates();	
    });


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
