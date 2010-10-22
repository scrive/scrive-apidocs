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

function newplacement(x, y, page, w, h, el) {
    return {x: x, y: y, page: page, w: w, h: h, el: el};
}

function placementToHTML(label, value) {
    var v = value;
    if(!v) {
	v = label;
    }
    return $("<div class='placedfield'>"
	     + v +
	     "</div>");
}

function placePlacements(pls, label, value) {
    $(pls).each(function () {
	    var pl = this;
	    var d = placementToHTML(label, value);
	    d.position({left: pl.x, top: pl.y});
	    $("#page" + pl.page).append(d);
	});
}

function newHiddenValue(label, value) {
    return $("<div style='display: none' class='" + label + "'>" + value + "</div>");
}

// this var will be kept in sync with the fields.

// var docstate;

function buildDraggableField(info, val) {
    var x = $("<div><span class='draghandle'>drag</span><input type='text' infotext='" + info + "' value='" + val + "' /></div>");
    x.draggable({ handle: ".draghandle"
		, helper: function (event) {
		return placementToHTML(info, val);
	    }
		});

    return x;
}

function buildDraggableText(val) {
    var x = $("<div><span class='draghandle'>drag</span> " + val + "</div>");
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

    if(author.name) {
	var aname = buildDraggableText(author.name);
	ad.append(aname);
    }
    if(author.company){
	var acomp = buildDraggableText(author.company);
	ad.append(acomp);
    }
    if(author.number) {
	var anumb = buildDraggableText(author.number);
	ad.append(anumb);
    }
    if(author.email) {
	var aemai = buildDraggableText(author.email);
	ad.append(aemai);
    }

    // other fields

    $(author.otherfields).each(function (){
	    var fd = this;
	    var field = buildDraggableField(fd.label, fd.value);
	    ad.append(field);
	});

    placePlacements(author.nameplacements, "Avsändare namn på motpart", author.name);
    placePlacements(author.companyplacements, "Avsändare  titel, företag", author.company);
    placePlacements(author.numberplacements, "Avsändare Orgnr/Persnr", author.number);
    placePlacements(author.emailplacements, "Avsändare personens e-mail", author.email);

    $(author.otherfields).each(function (){
	    var fd = this;
	    placePlacements(fd.placements, fd.label, fd.value);
	});

    var signatories = docstate.signatories;
    var sl = $("#signatorylist");
    sl.html("");

    $(signatories).each(function () {
	    signatoryToHTML(this);
	});
}

function signatoryToHTML(sig) {
    var sl = $("#signatorylist");
    var sigid = newUUID();
    sig.id = sigid;
    
    var sigentry = $("<div class='sigentry'></div>");
    
    
    var d = $("<div class='fields'></div>");
    
    var aname = buildDraggableField("Namn på motpart", sig.name);
    var acomp = buildDraggableField("Titel, företag", sig.company);
    var anumb = buildDraggableField("Orgnr/Persnr", sig.number);
    var aemai = buildDraggableField("Personens e-mail", sig.email);
    
    d.append(aname);
    d.append(acomp);
    d.append(anumb);
    d.append(aemai);
    
    // other fields
    
    $(sig.otherfields).each(function (){
	    var fd = this;
	    var field = buildDraggableField(fd.label, fd.value);
	    d.append(field);
	});
    
    sl.append(sigentry);
    
    var removeLink = $("<small><a href='#'>Ta bort</a></small>");
    removeLink.find("a").click(function () {
	    var ind = -1;
	    for(i = 0; i < docstate.signatories.length; i++){
		if(docstate.signatories[i].id = sigid) {
		    ind = i;
		    break;
		}
	    }
	    
	    docstate.signatories.splice(ind, 1);
	    var link = $(this);
	    link.parent().parent().detach();
	    return false;
	});
    sigentry.append(d);
    sigentry.append(removeLink);
    
    placePlacements(sig.nameplacements, "Namn på motpart", sig.name);
    placePlacements(sig.companyplacements, "Titel, företag", sig.company);
    placePlacements(sig.numberplacements, "Orgnr/Persnr", sig.number);
    placePlacements(sig.emailplacements, "Personens e-mail", sig.email);
    
    $(sig.otherfields).each(function (){
	    var fd = this;
	    placePlacements(fd.placements, fd.label, fd.value);
	});
}

signatoryadd = function() {
    var sig = newsignatory();
    docstate.signatories.push(sig);
    signatoryToHTML(sig);
    enableInfoText();
    return false;
}

$(document).ready(function () {
	docstateToHTML();
	

	enableInfoText();
    });