function placementToHTML(label, value) {
    var v = value;
    if(!v) {
	v = label;
    }
    return $("<div class='placedfield'>"
	     + "<span class='value'>" + v + "</span>" 
	     + "</div>");
}

function placePlacements(pls, label, value) {
    $(pls).each(function () {
	    var pl = this;
	    var d = placementToHTML(label, value);
	    var page = $("#page" + pl.page);
	    console.log(pl.y);
	    d.offset({left: pl.x, top: pl.y});

	    page.append(d);
	    
	});
}

function docstateToHTML(){
    // author first
    var author = docstate.author;

    // other fields

    $(author.otherfields).each(function (){
	    var fd = this;
	    placePlacements(fd.placements, fd.label, fd.value);
	});

    placePlacements(author.nameplacements, localization.senderName, author.name);
    placePlacements(author.companyplacements, localization.senderTitle, author.company);
    placePlacements(author.numberplacements, localization.senderNumber, author.number);
    placePlacements(author.emailplacements, localization.senderEmail, author.email);
                                
    var signatories = docstate.signatories;
    $(signatories).each(function () {
	    signatoryToHTML(this);
	});

}


function signatoryToHTML(sig) {
    $(sig.otherfields).each(function (){
	    var fd = this;
	    placePlacements(fd.placements, fd.label, fd.value);
	});

    placePlacements(sig.nameplacements, localization.partnerName , sig.name);
    placePlacements(sig.companyplacements, localization.partnerTitle, sig.company);
    placePlacements(sig.numberplacements, localization.partnerNumber , sig.number);
    placePlacements(sig.emailplacements,  localization.partnerEmail, sig.email);
}

function initializeTemplates () {

    if($(".pagediv").size() == 0){
	setTimeout("initializeTemplates();", 100);
	return;
    }

    //$("#loading-message").css({ display: "none" });
    //$("#edit-bar").css({ display: "" });

    docstateToHTML();
}

$(document).ready(function () {
	//$("#loading-message").css({ display: "" });
	//$("#edit-bar").css({ display: "none" });

	initializeTemplates();	
    });