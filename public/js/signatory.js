

var debug = false;

function newUUID() {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
	    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
	    return v.toString(16);
	}).toUpperCase();
}

function newsignatory() {
    return {name: "", company: "", number: "", email: "", 
	    nameplacements: [],
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
    return $("<div class='placedfield'>"
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
    var x = $("<div class='dragfield'><input type='text' name='fieldvalue' autocomplete='off' /><span class='status'></span></div>");

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

    var author = docstate.author;

    placePlacements(author.nameplacements, "Namn", author.name, "author", "name");
    placePlacements(author.emailplacements, "email", author.email, "author", "email");
    placePlacements(author.companyplacements, "company", author.company, "author", "sigco");
    placePlacements(author.numberplacements, "number", author.number, "author", "signr");


    var currentsig;
    var currentsigdiv = $("<span />");

    $(".signatory").each(function() {
	    var f = $(this);
	    if(f.text().indexOf(useremail) > -1) { // hacky; TODO add actual divs with classnames
		currentsigdiv = f;
	    }
	});

    var fields = currentsigdiv.find(".signatoryfields");


    $(docstate.signatories).each(function() {
	    var s = this;
	    s.id = newUUID();
	    if(s.email === useremail) {
		currentsig = s;
	    }

	    placePlacements(this.nameplacements, "Namn på motpart", this.name, s.id, "name");
	    placePlacements(this.emailplacements, "Personens e-mail", this.email, s.id, "email");
	    placePlacements(this.companyplacements, "Titel, företag", this.company, s.id, "sigco");
	    placePlacements(this.numberplacements, "Orgnr/Persnr", this.number, s.id, "signr");


	    $(s.otherfields).each(function () {
		    var f = this;
		    f.id = newUUID();
		    placePlacements(f.placements, f.label, f.value, s.id, f.id);
		});
	});

    if(currentsig.company.length === 0 && currentsig.companyplacements.length > 0) {
	var cfield = buildField("Titel, företag", currentsig.company, "sig");
	setFieldID(cfield, "sigco");
	setSigID(cfield, currentsig.id);
	setHiddenField(cfield, "fieldname", "sigco");
	fields.append(cfield);
    }

    if(currentsig.number.length === 0 && currentsig.numberplacements.length > 0) {
	var nfield = buildField("Orgnr/Persnr", currentsig.number, "sig");
	setFieldID(nfield, "signr");
	setSigID(nfield, currentsig.id);
	setHiddenField(nfield, "fieldname", "signr");
	fields.append(nfield);
    }

    $(currentsig.otherfields).each(function () {
	    var f = this;
	    if(f.value.length === 0){
		ofield = buildField(f.label, f.value, "sig");
		setFieldID(ofield, f.id);
		setSigID(ofield, currentsig.id);
		setHiddenField(ofield, "fieldname", f.label);
		fields.append(ofield);
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
	if(getValue(field)) {
	    setIcon(field, "done");
	} else {
	    setIcon(field, "athr");
	}
    } else if(type == "sig") {
	if(getValue(field)) {
	    // it's a signatory field, but it's filled out
	    setIcon(field, "done");
	} else if(getPlacedFieldsForField(field).size()) {
	    // if it's placed
	    setIcon(field, "sig");
	} else {
	    // not placed, so won't send
	    setIcon(field, "none");
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

function signatoryToHTML(sig) {
    var sl = $("#signatorylist");
    var sigid = newUUID();
    sig.id = sigid;
    
    var sigentry = $("<div class='sigentry'></div>");
    sigentry.append(newHiddenField("sigid", sigid));

    var d = $("<div class='fields'></div>");
    
    var aname = buildDraggableField("Namn på motpart", sig.name, "author");
    var acomp = buildDraggableField("Titel, företag", sig.company, "sig");
    var anumb = buildDraggableField("Orgnr/Persnr", sig.number, "sig");
    var aemai = buildDraggableField("Personens e-mail", sig.email, "author", true);

    setFieldName(aname, "signatoryname");
    setFieldName(acomp, "signatorycompany");
    setFieldName(anumb, "signatorynumber");
    setFieldName(aemai, "signatoryemail");
    
    setFieldID(aname, "name");
    setFieldID(acomp, "company");
    setFieldID(anumb, "number");
    setFieldID(aemai, "email");

    setSigID(aname, sigid);
    setSigID(acomp, sigid);
    setSigID(anumb, sigid);
    setSigID(aemai, sigid);

    aemai.find("input").attr('id', 'othersignatoryemail');
    if(aemai.find("input").attr("value")) {
	setIcon(aemai, "done");
    }

    d.append(aemai);
    d.append(aname);
    d.append(acomp);
    d.append(anumb);
    
    // other fields

    $(sig.otherfields).each(function (){
	    var fd = this;
	    var field = buildDraggableField(fd.label, fd.value, "sig");
	    var fieldid = newUUID();
	    fd.id = fieldid;
	    setFieldID(field, fieldid);
	    setSigID(field, sigid);
	    setFieldName(field, "fieldvalue");
	    setHiddenField(field, "fieldname", fd.label);
	    d.append(field);
	    placePlacements(fd.placements, fd.label, fd.value, sig.id, fd.id);
	});

    var newFieldLink = $("<small><a href='#'>New Field</a></small><br />");
    newFieldLink.find("a").click(function () {
	    var field = $("<div class='newfield'><input type='text' infotext='Type Field Name' /><input type='submit' value='ok'></div>");
	    field.find("input[type='submit']").click(function () {
		    fieldname = field.find("input[type='text']").attr("value");
		    if(fieldname == "Type Field Name" || fieldname == "") {
			return false;
		    }
		    var f = buildDraggableField(fieldname, "", "sig");
		    setFieldName(f, "fieldvalue");
		    var fieldid = newUUID();
		    setFieldID(f, fieldid);
		    setSigID(f, sigid);
		    setHiddenField(f, "fieldname", fieldname);
		    d.append(f);
		    enableInfoText(f);
		    updateStatus(f);
		    field.detach();
		    return false;
		});
	    d.append(field);
	    enableInfoText(field);
	    return false;
	});
    
    
    
    var removeLink = $("<small><a href='#'>Ta bort</a></small>");
    removeLink.find("a").click(function () {
	    var link = $(this);
	    detachFieldsForSig(sigid);
	    link.parents(".sigentry").detach();
	    return false;
	});


    sigentry.append(d);
    sigentry.append(newFieldLink);
    sigentry.append(removeLink);

    sl.append(sigentry);

    sigentry.hide();
    sigentry.slideDown("slow");
    
    placePlacements(sig.nameplacements, "Namn på motpart", sig.name, sigid, "name");
    placePlacements(sig.companyplacements, "Titel, företag", sig.company, sigid, "company");
    placePlacements(sig.numberplacements, "Orgnr/Persnr", sig.number, sigid, "number");
    placePlacements(sig.emailplacements, "Personens e-mail", sig.email, sigid, "email");
    
}

signatoryadd = function() {
    var sig = newsignatory();
    signatoryToHTML(sig);
    enableInfoText();
    return false;
};

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
	
    enableInfoText();

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