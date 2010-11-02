// template field dragging system

// include this file to start the dragging functionality

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
	    console.log(pl.y);
	    d.offset({left: pl.x, top: pl.y});

	    page.append(d);
	    
	    setSigID(d, sigid);
	    setFieldID(d, fieldid);
	    setHiddenField(d, "placedx", String(Math.round(d.position().left)));
	    setHiddenField(d, "placedy", String(Math.round(d.position().top)));
	    setHiddenField(d, "placedpage", String(pl.page));
	    setHiddenField(d, "placedwidth", String(Math.round(page.width())));
	    setHiddenField(d, "placedheight", String(Math.round(page.height())));



	    d.draggable({ 
		    appendTo: "body",
			helper: function (event, ui) {
			return placementToHTML(label, value);
		    },
			start: function (event, ui) {
			$(this).css({ display: "none" });
		    }
		
		});
	});
}

function newHiddenValue(label, value) {
    return $("<span style='display: none' class='" + label + "'>" + value + "</span>");
}

function getHiddenValue(field, label) {
    var s = $(field).find("." + label);
    if(s.size()){
	return s.text();
    }
    console.log("field has no value named " + label);
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
    }
    console.log("field has no hidden field called " + label);
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
	console.log("here");
	var s = $(field).find("input[type='text'], input[type='email']");
	if(s.size()) {
	    if(s.attr("value") == s.attr("infotext")) {
		return "";
	    } else {
		return s.attr("value");
	    }
	} else {
	    console.log("field has no input box");
	}
    } else if(isDraggableText(field)) {
	return getHiddenValue(field, "fieldvalue");
    } else if(isPlacedField(field)) {
	return getHiddenValue(field, "value");
    } else {
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
    } else {
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
    } else {
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
    } else {
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
    var x = $("<div class='dragfield'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span><input type='text' autocomplete='off' /><span class='status'></span></div>");
    if(emailp) {
	x = $("<div class='dragfield'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span><input type='email' autocomplete='off' /><span class='status'></span></div>");
    }


    setInfotext(x, info);
    setValue(x, val);
    setFieldType(x, type);

    x.draggable({ handle: ".draghandle"
		, helper: function (event) {
		var field = $(this);
		var input = field.find("input");
		return placementToHTML(input.attr("infotext"), input.attr("value"));
	    }
		});

    var input = x.find("input");

    input.keydown(magicUpdate);
    input.keyup(magicUpdate);
    input.change(magicUpdate);
    return x;
}

// for use with author fields
function buildDraggableText(val) {
    var x = $("<div class='dragtext'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span> <span class='fieldvalue'>" + val + "</span></div>");
    x.draggable({ handle: ".draghandle"
		, helper: function (event) {
		return placementToHTML("abc", val);
	    }
		});

    return x;
}

// destroy the HTML that's there to sync it with the docstate
function docstateToHTML(){
    // author first
    var author = docstate.author;

    var ad = $("#authordetails");

    ad.html("");
    ad.append(newHiddenValue("sigid", "author"));
    var fds = $("<div class='fields'></div>");

    if(author.name) {
	var aname = buildDraggableText(author.name);
	setFieldID(aname, "name");
	setSigID(aname, "author");
	setFieldType(aname, "text");
	fds.append(aname);
    }
    if(author.company){
	var acomp = buildDraggableText(author.company);
	setFieldID(acomp, "name");
	setSigID(acomp, "author");
	setFieldType(acomp, "text");
	fds.append(acomp);
    }
    if(author.number) {
	var anumb = buildDraggableText(author.number);
	setFieldID(anumb, "name");
	setSigID(anumb, "author");
	setFieldType(anumb, "text");
	fds.append(anumb);
    }
    if(author.email) {
	var aemai = buildDraggableText(author.email);
	setFieldID(aemai, "name");
	setSigID(aemai, "author");
	setFieldType(aemai, "text");
	fds.append(aemai);
    }

    // other fields

    $(author.otherfields).each(function (){
	    var fd = this;
	    fd.id = newUUID();
	    var field = buildDraggableField(fd.label, fd.value, "author");
	    setFieldName(field, "fieldvalue");
	    setFieldID(field, fd.id);
	    setSigID(field, "author");
	    setHiddenField(field, "fieldname", fd.label);
	    fds.append(field);
	    placePlacements(fd.placements, fd.label, fd.value, "author", fd.id);
	});

    var newFieldLink = $("<small><a href='#'>New Field</a></small><br />");
    newFieldLink.find("a").click(function () {
	    var field = $("<div class='newfield'><input type='text' infotext='Type Field Name' /><input type='submit' value='ok'></div>");
	    field.find("input[type='submit']").click(function () {
		    var fieldname = field.find("input[type='text']").attr("value");
		    if(fieldname == "Type Field Name" || fieldname == "") {
			return false;
		    }
		    var f = buildDraggableField(fieldname, "", "author");
		    setFieldName(f, "fieldvalue");
		    var fieldid = newUUID();
		    setFieldID(f, fieldid);
		    setSigID(f, "author");
		    setHiddenField(f, "fieldname", fieldname);
		    fds.append(f);
		    enableInfoText(f);
		    updateStatus(f);
		    field.detach();
		    return false;
		});
	    fds.append(field);
	    enableInfoText(field);
	    return false;
	});

    ad.append(fds);
    ad.append(newFieldLink);

    placePlacements(author.nameplacements, "Avsändare namn på motpart", author.name, "author", "name");
    placePlacements(author.companyplacements, "Avsändare  titel, företag", author.company, "author", "company");
    placePlacements(author.numberplacements, "Avsändare Orgnr/Persnr", author.number, "author", "number");
    placePlacements(author.emailplacements, "Avsändare personens e-mail", author.email, "author", "email");

    var signatories = docstate.signatories;
    var sl = $("#signatorylist");
    sl.html("");

    $(signatories).each(function () {
	    signatoryToHTML(this);
	});

    $(".dragfield").each(function () {
	    updateStatus(this);
	});
}

function getIcon(field){
    if(isDraggableField(field)){
	return getHiddenValue(field, "status");
    } else {
	console.log(getFieldType(field) + " does not have an icon, cannot get");
	return "";
    }
}

function setIcon(field, status) {
    if(isDraggableField(field)) {
	setHiddenValue(field, "status", status);
    } else {
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
    } else {
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
    } else {
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
	console.log("author field: " + getFieldName(field));
	if(getValue(field)) {
	    setIcon(field, "done");
	} else {
	    setIcon(field, "athr");
	}
    } else if(type == "sig") {
	console.log("author field: " + getFieldName(field));
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
    } else {
	console.log("field has bad field type: " + getFieldName(field));
	alert("bad field type");
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

    d.append(aname);
    d.append(aemai);

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

function makeDropTargets() {
    $(".pagediv").droppable({ drop: function(event, ui) {
		var page = $(this);
		var field = $(ui.draggable);
		var helper = $(ui.helper);

		var top = helper.offset().top - page.offset().top + window.pageYOffset;
		var left = helper.offset().left - page.offset().left + window.pageXOffset;

		var pageno = parseInt(page.attr("id").substr(4));

		var sigid = getSigID(field);
		var fieldid = getFieldID(field);
		var pl = newplacement(left, top, pageno, page.width(), page.height());
		placePlacements([pl], getLabel(field), getValue(field), sigid, fieldid);
		
	
		if(isPlacedField(field)) {
		    field.remove();
		    helper.remove();
		} else {
		    updateStatus(field);
		}
		
		
	    }});

    return true;
}

function initializeTemplates () {

    if($(".pagediv").size() == 0){
	setTimeout("initializeTemplates();", 100);
	return;
    }

    $("#loading-message").css({ display: "none" });
    $("#edit-bar").css({ display: "" });

    docstateToHTML();
	
    enableInfoText();
    
    makeDropTargets();


    $("form").submit(function () {
	    var form = $("form");
	    $(".placedfield input[type='hidden']").each(function () {
		    var h = $(this);
		    console.log(h.attr('name') + ": " + h.attr('value'));
		    form.append(h);
		});
	});
}

$(document).ready(function () {
	$("#loading-message").css({ display: "" });
	$("#edit-bar").css({ display: "none" });

	initializeTemplates();	
    });