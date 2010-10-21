// template field dragging system

// include this file to start the dragging functionality

function newsignatory(el) {
    return {name: "", company: "", number: "", email: "", 
	    nameplacements: [],
	    companyplacements: [],
	    numberplacements: [],
	    emailplacements: [],
	    otherfields: [],

	    el: el};
}

function newplacement(x, y, page, w, h, el) {
    return {x: x, y: y, page: page, w: w, h: h, el: el};
}

function placementToHTML(label, value) {
    return $("<div class='placedfield'>"
	     + (value == null)?label:value +
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

// this var will be kept in sync with the fields.

// var docstate;

function buildDraggableField(info, val) {
    return $("<div><input type='text' infotext='" + info + "' value='" + val + "' /></div>");
}

// destroy the HTML that's there to sync it with the docstate
function docstateToHTML(){
    // author first
    var author = docstate.author;

    var ad = $("#authordetails");

    ad.html("");

    var aname = buildDraggableField("Namn på motpart", author.name);
    var acomp = buildDraggableField("Titel, företag", author.company);
    var anumb = buildDraggableField("Orgnr/Persnr", author.number);
    var aemai = buildDraggableField("Personens e-mail", author.email);

    ad.append(aname);
    ad.append(acomp);
    ad.append(anumb);
    ad.append(aemai);

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
	    var sig = this;
	    
	    var d = $("<div class='sigentry'></div>");

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

	    sl.append(d);

	    var removeLink = $("<small><a href='#'>Ta bort</a></small>");
	    sl.append(removeLink);
	    

	    placePlacements(sig.nameplacements, "Avsändare namn på motpart", sig.name);
	    placePlacements(sig.companyplacements, "Avsändare  titel, företag", sig.company);
	    placePlacements(sig.numberplacements, "Avsändare Orgnr/Persnr", sig.number);
	    placePlacements(sig.emailplacements, "Avsändare personens e-mail", sig.email);
	    
	    $(sig.otherfields).each(function (){
		    var fd = this;
		    placePlacements(fd.placements, fd.label, fd.value);
		});
	});
	    
    
}

$(document).ready(function () {
	docstateToHTML();


	enableInfoText();
    });