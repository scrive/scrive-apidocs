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
	    d.offset({left: pl.x, top: pl.y});
	    d.append(newHiddenValue("sigid", sigid));
	    d.append(newHiddenValue("fieldid", fieldid));
	    $("#page" + pl.page).append(d);

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

function newHiddenField(name, value) {
    return $("<input type='hidden' name='" + name + "' value='" + value + "' />");
}

function magicUpdate(){
    var field = $(this);
    var sigid = field.parents(".sigentry").find(".sigid").text();
    var fieldid = field.parent().find(".fieldid").text();
    var value = field.attr("value");

    $(".placedfield").each(function () {
	    var f = $(this);
	    if(f.find(".sigid").text() == sigid
	       && f.find(".fieldid").text() == fieldid) {
		f.find(".value").text(value);
	    }
	});
}

function buildDraggableField(info, val) {
    var x = $("<div><span class='draghandle'>drag</span><input type='text' infotext='" + info + "' value='" + val + "' /></div>");
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

function buildDraggableText(val) {
    var x = $("<div><span class='draghandle'>drag</span> <span class='fieldvalue'>" + val + "</span></div>");
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

    if(author.name) {
	var aname = buildDraggableText(author.name);
	aname.append(newHiddenValue("fieldid", "name"));
	ad.append(aname);
    }
    if(author.company){
	var acomp = buildDraggableText(author.company);
	acomp.append(newHiddenValue("fieldid", "company"));
	ad.append(acomp);
    }
    if(author.number) {
	var anumb = buildDraggableText(author.number);
	anumb.append(newHiddenValue("fieldid", "number"));
	ad.append(anumb);
    }
    if(author.email) {
	var aemai = buildDraggableText(author.email);
	aemai.append(newHiddenValue("fieldid", "email"));
	ad.append(aemai);
    }

    // other fields

    $(author.otherfields).each(function (){
	    var fd = this;
	    fd.id = newUUID();
	    var field = buildDraggableField(fd.label, fd.value);
	    field.append(newHiddenValue("fieldid", fd.id));
	    ad.append(field);

	    placePlacements(fd.placements, fd.label, fd.value, "author", fd.id);
	});

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
}

function signatoryToHTML(sig) {
    var sl = $("#signatorylist");
    var sigid = newUUID();
    sig.id = sigid;
    
    var sigentry = $("<div class='sigentry'></div>");
    sigentry.append(newHiddenValue("sigid", sigid));    
    sigentry.append(newHiddenField("sigid", sigid));

    var d = $("<div class='standardfields'></div>");
    
    var aname = buildDraggableField("Namn på motpart", sig.name);
    var acomp = buildDraggableField("Titel, företag", sig.company);
    var anumb = buildDraggableField("Orgnr/Persnr", sig.number);
    var aemai = buildDraggableField("Personens e-mail", sig.email);

    aname.find("input").attr("name", "signatoryname");
    acomp.find("input").attr("name", "signatorycompany");
    anumb.find("input").attr("name", "signatorynumber");
    aemai.find("input").attr("name", "signatoryemail");
    
    aname.append(newHiddenValue("fieldid", "name"));
    acomp.append(newHiddenValue("fieldid", "company"));
    anumb.append(newHiddenValue("fieldid", "number"));
    aemai.append(newHiddenValue("fieldid", "email"));

    d.append(aname);
    d.append(acomp);
    d.append(anumb);
    d.append(aemai);
    
    // other fields

    var of = $("<div class='otherfields'></div>");
    
    $(sig.otherfields).each(function (){
	    var fd = this;
	    var field = buildDraggableField(fd.label, fd.value);
	    field.find("input").attr("name", "fieldname");
	    var fieldid = newUUID();
	    fd.id = fieldid;
	    field.append(newHiddenValue("fieldid", fieldid));

	    field.append(newHiddenField("fieldid", fieldid));
	    field.append(newHiddenField("fieldsigid", sigid));
	    of.append(field);
	    placePlacements(fd.placements, fd.label, fd.value, sig.id, fd.id);
	});
    
    sl.append(sigentry);
    
    var removeLink = $("<small><a href='#'>Ta bort</a></small>");
    removeLink.find("a").click(function () {
	    var link = $(this);
	    link.parents(".sigentry").detach();
	    return false;
	});


    sigentry.append(d);
    sigentry.append(of);
    sigentry.append(removeLink);
    
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

function getValue(x) {
    var val = $(x).find(".fieldvalue").text();
    if(!val) {
	if(!$(x).find("input").hasClass("grayed")) {
	    val = $(x).find("input").attr("value");
	}
    }

    if (!val) {
	val = "";
    }

    return val;
}

function makeDropTargets() {
    $(".pagediv").droppable({ drop: function(event, ui) {
		var page = $(this);
		var field = $(ui.draggable);
		var helper = $(ui.helper);

		var top = helper.offset().top - page.offset().top;
		var left = helper.offset().left - page.offset().left;

		var pageno = parseInt(page.attr("id").substr(4));

		var fieldid = field.find(".fieldid").text();

		if(!field.hasClass("placedfield")) {
		    var sigid = field.parents(".sigentry").find(".sigid").text();
		    if(!sigid) {
			sigid = field.parents("#authordetails").find(".sigid").text();
		    }
		    
		    if(!sigid) {
			alert("no sig id");
		    }
		    var pl = newplacement(left, top, pageno, page.width(), page.height());
		    placePlacements([pl], getLabel(field), getValue(field), sigid, fieldid);
		} else {
		    var sigid = field.find(".sigid").text();
		    
		    var pl = newplacement(left, top, pageno, page.width(), page.height());
		    placePlacements([pl], "nolabel", field.find(".value").text(), sigid, fieldid);
		    field.remove();
		    helper.remove();
		}
	    }});

    return true;
}

function initializeTemplates () {

    if($(".pagediv").size() == 0){
	setTimeout("initializeTemplates();", 500);
	return;
    }

    $("#loading-message").css({ display: "none" });
    $("#edit-bar").css({ display: "" });
	

    docstateToHTML();
	
    enableInfoText();
    
    makeDropTargets();

    $("form").submit(function () {
	    $(".placedfield").each(function () {
		    var field = $(this);
		    var x = field.position().left;
		    var y = field.position().top;
		    var pageno = field.parent().attr("id").substr(4);
		    var pagew = field.parent().width();
		    var pageh = field.parent().height();
		    var sigid = field.find(".sigid").text();
		    var fieldid = field.find(".fieldid").text();
		    
		    $("form").append(newHiddenField("placedx", x));
		    $("form").append(newHiddenField("placedy", y));
		    $("form").append(newHiddenField("placedpage", pageno));
		    $("form").append(newHiddenField("placedwidth", pagew));
		    $("form").append(newHiddenField("placedheight", pageh));
		    $("form").append(newHiddenField("placedsigid", sigid));
		    $("form").append(newHiddenField("placedfieldid", fieldid));
		});

	});
}

$(document).ready(function () {
	$("#loading-message").css({ display: "" });
	$("#edit-bar").css({ display: "none" });
	

	initializeTemplates();	
    });