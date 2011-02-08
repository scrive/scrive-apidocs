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
	    setHiddenField(d, "placedx", String(Math.round(d.position().left)));
	    setHiddenField(d, "placedy", String(Math.round(d.position().top)));
	    setHiddenField(d, "placedpage", String(pl.page));
	    setHiddenField(d, "placedwidth", String(Math.round(page.width())));
	    setHiddenField(d, "placedheight", String(Math.round(page.height())));

	    // make it draggable
	    d.draggable({ 
	      // this makes sure it can move between divs
	      appendTo: "body",
              stop: function(event, ui) {
                var field = $(ui.draggable);
                var helper = $(ui.helper);
                console.log("helper");
                console.log(helper);
                console.log(field);
                console.log("nothing");
                // for some reason, this needs to be helper (field is
                // empty)
                if(isPlacedField(helper)) {
                  helper.remove();
                  console.log("removed field + helper");
                }
              },
	      // build a helper so it doesn't delete the original
	      helper: function (event, ui) {
		return placementToHTML(label, value);
	      },
	      // but we don't want to show the original so it looks like 
	      // you are dragging the original
	      start: function (event, ui) {
		$(this).css({ display: "none" });
	      },

	      
	    });
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

function buildDraggableField(info, val, type, emailp) {
    var x = $("<div class='dragfield'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span><input type='text' autocomplete='off' /><span class='dragstatus'></span> <span class='fillstatus'></span></div>");
    if(emailp) {
	x = $("<div class='dragfield'><span class='draghandle ui-icon ui-icon-arrowthick-1-w'>drag</span><input type='email' autocomplete='off' /><span class='dragstatus'></span> <span class='fillstatus'></span></div>");
    }


    setInfotext(x, info);
    setValue(x, val);
    setFieldType(x, type);

    x.draggable({ handle: ".draghandle"
		, zIndex: 10000
		, appendTo: "body"
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
		, zIndex: 10000
		, appendTo: "body"
		, helper: function (event) {
		return placementToHTML("abc", val);
	    }
		});

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
	} else {
	    setDragStatus(field, "not placed");
	}
	if(getValue(field)) {
	    setFillStatus(field, "filled");
	} else {
	    setFillStatus(field, "author");
	}
    } else if(type == "sig") {
	if(getPlacedFieldsForField(field).size()){
	    setDragStatus(field, "placed");
	} else if(isStandardField(field)) {
	    setDragStatus(field, "not placed");
	} else {
	    setDragStatus(field, "must place");
	}
	if(getValue(field)) {
	    // it's a signatory field, but it's filled out
	    setFillStatus(field, "done");
	} else if(getPlacedFieldsForField(field).size()) {
	    // if it's placed
	    setFillStatus(field, "sig");
	} else {
	    // not placed, so won't send
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


function authorToHTML(sig) {
  var sl = $("#personpane");
  var sigid = "author";
  sig.id = sigid;

  var sigentry = sl.find(".authordetails");

  var manlink = sigentry.find("a.man");

  manlink.click(function(){
    console.log("man click");
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
    

  var d = sigentry.find(".fields");
  var of = sigentry.find(".otherfields");
    
  var aname = sigentry.find(".authorname");
  var acomp = sigentry.find(".authorcomp");
  var anumb = sigentry.find(".authornum");
  var aemai = sigentry.find(".authoremail");

    setSigID(aname, sigid);
    setSigID(acomp, sigid);
    setSigID(anumb, sigid);
    setSigID(aemai, sigid);

    setValue(aname, sig.name);
    setValue(acomp, sig.company);
    setValue(anumb, sig.number);
    setValue(aemai, sig.email);

  if(sig.name === "") {
    aname.remove();
  }

  if(sig.company === "") {
    acomp.remove();
  }

  if(sig.number === "") {
    anumb.remove();
  }

  if(sig.email === "") {
    aemai.remove();
  }

    sigentry.find(".dragtext").draggable({ handle: ".draghandle"
		                            , zIndex: 10000
		                            , appendTo: "body"
		                            , helper: function (event) {
		                              var field = $(this);
		                              
		                              return placementToHTML(getValue(field));
	    }
		});

    
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

      field.draggable({ handle: ".draghandle"
			, zIndex: 10000
			, appendTo: "body"
			, helper: function (event) {
			  var field = $(this);
			  var input = field.find("input");
			  return placementToHTML(input.attr("infotext"), input.attr("value"));
		        }
		      });
      
      var input = field.find("input");

      input.keydown(magicUpdate);
      input.keyup(magicUpdate);
      input.change(magicUpdate);

      of.append(field);
	    //	    
    });

  sigentry.find("a.plus").click(function () {
    var field = $("<div class='newfield'><input class='newfieldbox' type='text' infotext='Type Field Name' /><a href='#' class='plus'></a><a href='#' class='minus'></a></div>");
    field.find("a.minus").click(function() {
      field.remove();
      return false;
    });
    field.find("a.plus").click(function () {
      fieldname = field.find("input[type='text']").attr("value");
      if(fieldname == "Type Field Name" || fieldname == "") {
	return false;
      }
      var f = $("#templates .customfield").first().clone();
      console.log(f);
      //var f = buildDraggableField(fieldname, "", "sig");
      setInfotext(f, fieldname);
      setValue(f, "");
      setFieldType(f, "sig");
      
      f.draggable({ handle: ".draghandle"
		    , zIndex: 10000
		    , appendTo: "body"
		    , helper: function (event) {
		      var field = $(this);
		      var input = field.find("input");
		      return placementToHTML(input.attr("infotext"), input.attr("value"));
		    }
		  });
      
      var input = f.find("input");
      
      input.keydown(magicUpdate);
      input.keyup(magicUpdate);
      input.change(magicUpdate);

      setFieldName(f, "fieldvalue");
      var fieldid = newUUID();
      setFieldID(f, fieldid);
      setSigID(f, sigid);
      setHiddenField(f, "fieldname", fieldname);

      f.find("a.minus").click(function() {
	//console.log(f);
	
	var ff = getPlacedFieldsForField(f);
	//console.log(ff);
	ff.each(function(){this.remove();});
	f.remove();
      });
		    
      of.append(f);
      enableInfoTextOnce(f);
      updateStatus(f);
		    

      field.detach();
		    
      return false;
    });
    of.append(field);
    enableInfoTextOnce(field);
    return false;
  });
    
  $("#peopleList ol").append("<li><a href='#'>" + sig.name + " (Author)</a></li>");
}


function signatoryToHTML(sig) {
    //console.log("adding signatory");
    var sl = $("#personpane");
    var sigid = newUUID();
    sig.id = sigid;

    if(sig.email === docstate.author.email){
      sig.author = true;
    }

    var sigentry = $('#templates .persondetails').first().clone();

  if(sig.author){
    sigentry.addClass("authorentry");
    var manlink = $("<a class='man' href='#'> </a>");
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

    sigentry.find(".signStepsBodyIcons .man").remove();
    sigentry.find(".signStepsBodyIcons").prepend(manlink);

    sigentry.find(".partyrole input").first().attr("checked", "true");

  }

  
    
    //var sigentry = $("<div class='sigentry'></div>");
    //sigentry.append(newHiddenField("sigid", sigid));
    setHiddenField(sigentry, "sigid", sigid);

    var d = sigentry.find(".fields");
    var of = sigentry.find(".otherfields");
    
    var aname = sigentry.find(".signame");
    var acomp = sigentry.find(".sigcomp");
    var anumb = sigentry.find(".signum");
    var aemai = sigentry.find(".sigemail");

    setSigID(aname, sigid);
    setSigID(acomp, sigid);
    setSigID(anumb, sigid);
    setSigID(aemai, sigid);

    setValue(aname, sig.name);
    setValue(acomp, sig.company);
    setValue(anumb, sig.number);
    setValue(aemai, sig.email);

    sigentry.find(".dragfield").draggable({ handle: ".draghandle"
		                            , zIndex: 10000
		                            , appendTo: "body"
		                            , helper: function (event) {
		                              var field = $(this);
		                              var input = field.find("input");
		                              
		                              return placementToHTML(input.attr("infotext"), input.attr("value"));
	    }
		});

    sigentry.find("input").keydown(magicUpdate);
    sigentry.find("input").keyup(magicUpdate);
    sigentry.find("input").change(magicUpdate);

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

	    field.draggable({ handle: ".draghandle"
			      , zIndex: 10000
			      , appendTo: "body"
			, helper: function (event) {
			var field = $(this);
			var input = field.find("input");
			return placementToHTML(input.attr("infotext"), input.attr("value"));
		    }
		});

	    var input = field.find("input");

	    input.keydown(magicUpdate);
	    input.keyup(magicUpdate);
	    input.change(magicUpdate);

	    of.append(field);
	    //	    
	});

    sigentry.find("a.plus").click(function () {
      var field = $("<div class='newfield'><input class='newfieldbox' type='text' infotext='Type Field Name' /><a href='#' class='okIcon'></a><a href='#' class='minus'></a></div>");
      field.find("a.minus").click(function() {
        field.remove();
        return false;
      });

      field.find("a.okIcon").click(function () {
		    fieldname = field.find("input[type='text']").attr("value");
		    if(fieldname == "Type Field Name" || fieldname == "") {
			return false;
		    }
		    var f = $("#templates .customfield").first().clone();
		    console.log(f);
		    //var f = buildDraggableField(fieldname, "", "sig");
		    setInfotext(f, fieldname);
		    setValue(f, "");
		    setFieldType(f, "sig");

		    f.draggable({ handle: ".draghandle"
				, zIndex: 10000
				, appendTo: "body"
				, helper: function (event) {
				var field = $(this);
				var input = field.find("input");
				return placementToHTML(input.attr("infotext"), input.attr("value"));
			    }
			});

		    var input = f.find("input");
		    
		    input.keydown(magicUpdate);
		    input.keyup(magicUpdate);
		    input.change(magicUpdate);

		    setFieldName(f, "fieldvalue");
		    var fieldid = newUUID();
		    setFieldID(f, fieldid);
		    setSigID(f, sigid);
		    setHiddenField(f, "fieldname", fieldname);

		    f.find("a.minus").click(function() {
			    //console.log(f);
			    
			    var ff = getPlacedFieldsForField(f);
			    //console.log(ff);
			    ff.each(function(){this.remove();});
			    f.remove();
			});
		    
		    of.append(f);
		    enableInfoTextOnce(f);
		    updateStatus(f);
		    

		    field.detach();
		    
		    return false;
		});
	    of.append(field);
	    enableInfoTextOnce(field);
	    return false;
	});
    
    
    
    enableInfoTextOnce(sigentry);


    //sigentry.append(d);
    //sigentry.append(newFieldLink);
    //sigentry.append(removeLink);



    //sigentry.hide();
    //sigentry.slideDown("slow");
    
    var n = "Unnamed";

    if(sig.name == "") {
	n = "(fill in)";
    } else {
	n = sig.name;
    }
    
    $("#peopleList ol").append("<li><a href='#'>" + n + "</a></li>");
    sl.append(sigentry);
    //currentSig = sigentry;
    //    sigentry.hide();
    // $("#personpane").children().hide().first().show();
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

function makeDropTargets() {
  $("#signStepsContainer").droppable({ drop: function(event, ui) {
    var field = $(ui.draggable);
    var helper = $(ui.helper);
    console.log(field);
    if(isPlacedField(field)) {
      field.remove();
      helper.remove();
    }

  }});

    $(".pagediv").droppable({ drop: function(event, ui) {
		var page = $(this);
		var field = $(ui.draggable);
		var helper = $(ui.helper);

                var windowScrollY = window.pageYOffset ;
                if (windowScrollY == undefined) windowScrollY = (document.documentElement.scrollTop?document.documentElement.scrollTop:document.body.scrollTop); 
                var windowScrollX =  window.pageXOffset ;
                if (windowScrollX == undefined) windowScrollX = (document.documentElement.scrollLeft?document.documentElement.scrollLeft:document.body.scrollLeft); 

		var top = helper.offset().top - page.offset().top + windowScrollY;
		var left = helper.offset().left - page.offset().left + windowScrollX;             
                
		var pageno = parseInt(page.attr("id").substr(4));

		var sigid = getSigID(field);
		var fieldid = getFieldID(field);
		var pl = newplacement(left, top, pageno, page.width(), page.height());
		placePlacements([pl], getLabel(field), getValue(field), sigid, fieldid);
		
      console.log(field);
	
		if(isPlacedField(field)) {
		    field.remove();
		    helper.remove();
		} else {
		    updateStatus(field);
		}
		
		return false;
	    }});

    return true;
}

function initializeTemplates () {

    if($(".pagediv").size() == 0){
	setTimeout("initializeTemplates();", 100);
	return;
    }

    //$("#loading-message").css({ display: "none" });
    //$("#edit-bar").css({ display: "" });

    placePlacementsOfSignatories(docstate.signatories);
	
    enableInfoText();
    
    makeDropTargets();

  $("#personpane").children().removeClass("currentPerson").last().addClass("currentPerson");

    $("form").submit(function () {
	    var form = $("form");
	    $(".placedfield input[type='hidden']").each(function () {
		    var h = $(this);
		    form.append(h);
		});
	});
}

$(document).ready(function () {
	//$("#loading-message").css({ display: "" });
	//$("#edit-bar").css({ display: "none" });
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

