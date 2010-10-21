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

// this var will be kept in sync with the fields.

// var docstate;

function docstateToHTML(){
    // author first
    var author = docstate.author;
    
    $("#authorname").attr("value", author.name);
    $("#authorcompany").attr("value", author.company);
    $("#authornumber").attr("value", author.number);
    $("#authoremail").attr("value", author.email);

    $(author.nameplacements).each(function () {
	    var pl = this;
	    var d = placementToHTML("Author Name", author.name);
	    // set offset of d
	    $("#page" + pl.page).append(d);
	});

    var signatories = docstate.signatories;

    
}