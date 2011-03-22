// WebSigner Plugin for Nordea
function hasIESigner1Plugin() {
  try { 
	var xObj = new ActiveXObject( Nexus.SignerCtl ); 
	if(xObj) { 
	  return true;
	} 
  } catch (e) { 
	// do nothing
  }
  return false;
}

function IEInstallSigner1Object() {
  $('body').append("<OBJECT ID='signer' CLASSID='CLSID:6969E7D5-223A-4982-9B79-CC4FAC2D5E5E'> </OBJECT>");
}

function hasMozillaSigner1Plugin() {
  if(navigator.plugins) {
	if (navigator.plugins.length > 0) {
	  if (navigator.mimeTypes && navigator.mimeTypes["application/x-personal-signer"]) {
		if (navigator.mimeTypes["application/x-personal-signer"].enabledPlugin) {
		  return true;
		}
	  }
	}
  }
  return false;
}

function mozillaInstallSigner1Object() {
  $('body').append('<OBJECT id="signerId" type="application/x-personal-signer" length=0 height=0>');
}

function doSign1(tbs, posturl) {
  var signer = document.getElementById('signerId');
  signer.SetMimeType('text/plain');
  signer.SetCharacterEncoding('platform');
  signer.SetFormat('PKCS7SIGNED_Attached');
  signer.SetFileName('skrivapa.txt');
  signer.SetWindowName('_self');
  signer.SetDataToBeSigned(escape(tbstext));
  signer.SetSignReturnName('SignedData');
  signer.SetDataReturnName('UnsignedData');
  signer.SetVersionReturnName('Version');
  signer.SetIssuers('');
  signer.SetSubjects('');
  signer.SetViewData('true');
  signer.SetIncludeCaCert('true');
  signer.SetIncludeRootCaCert('true');
  if (signer.Sign() == 0) {
	return signer.GetSignature();
  } else {
	console.log(signer.GetErrorString());
  }
  return null;
}



function sign1Success(transactionid, tbs, nonce, servertime, posturl, formselector) {
  if($.browser.msie) {
	if(hasIESigner1Plugin()) {
	  IEInstallSigner1Object();
	} else {
      flashNordeaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasMozillaSigner1Plugin()){
	  mozillaInstallSigner1Object();
	} else {
	  flashNordeaMessage();
	}
  }else {
	alert("Unsupported browser.");
	return false;
  }
  var sig = doSign1(tbs);
  if(sig) {
    displayLoadingOverlay("Verifying signature . . .");
	var form = $(formselector);
	form.find("#signatureinput").val(sig);
	form.find("#transactionidinput").val(transactionid);
    form.find("#elegprovider").val("nordea");
    form.attr("action", posturl);
	form.submit();
  } else {
	alert("could not sign");
  }
}

function doSign1(tbs) {
  if($.browser.msie) {
	if(hasIESigner1Plugin()) {
	  IEInstallSigner1Object();
	  return IESign1(tbs);
	} else {
	  flashNordeaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasMozillaSigner1Plugin()){
	  mozillaInstallSigner1Object();
	  return mozillaSign1(tbs);
	} else {
	  flashNordeaMessage();
	}
  } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  return null;
}

function sign1(ajaxurl, formselector, posturl) {
  var good = false;
  if($.browser.msie) {
	if(hasIESigner1Plugin()) {
	  good = true;
	} else {
	  flashNordeaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasMozillaSigner1Plugin()){
	  good = true;
	} else {
	  flashNordeaMessage();
	}
  } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  if(!good){
	return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
		  'success': function(data){
		    if(data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      sign1Success(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
		      alert("OOPS");
		    }
		    
	      },
		  error: function(){ alert("oh no!");}});
  return false;
}

function sign1Author(ajaxurl, formselector, posturl) {
  var good = false;
  if($.browser.msie) {
	if(hasIESigner1Plugin()) {
	  good = true;
	} else {
	  flashNordeaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasMozillaSigner1Plugin()){
	  good = true;
	} else {
	  flashNordeaMessage();
	}
  } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  if(!good){
	return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
          'data': {'tbs': getTBS()},
		  'success': function(data){
		    if(data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      sign1Success(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
		      alert("OOPS");
		    }
		    
	      },
		  error: function(){ alert("oh no!");}});
  return false;
}

function flashNordeaMessage() {
  addFlashMessage("Du har inte Nordeas e-legitimation installerad. Du kan ladda ned Nordeas e-legitimation från Nordeas internetbank.");
}

// Signer2 plugin for BankID

function hasSign2PluginIE() {
  try {
	var xObj = new ActiveXObject("Nexus.SignerV2Ctl");
	if(xObj) {
	  return true;
	}
  } catch (e) {
	// do nothing
  }
  return false;
}

function installSign2IE() {
  $("body").append('<OBJECT ID="signer2" CLASSID="CLSID:FB25B6FD-2119-4cef-A915-A056184C565E"></OBJECT>');
}

function hasSign2PluginMozilla() {
  if(navigator.plugins) {
	if (navigator.plugins.length > 0) {
	  if (navigator.mimeTypes && navigator.mimeTypes["application/x-personal-signer2"]) {
		if (navigator.mimeTypes["application/x-personal-signer2"].enabledPlugin) {
		  return true;
		}
	  }
	}
  }
  return false;
}

function installSign2Mozilla() {
  $("body").append('<OBJECT id="signer2" type="application/x-personal-signer2" length=0 height=0></OBJECT>');
}



function doSign2 (tbs, nonce, servertime) {
  //    var signer2 = $("#signer2").get(0);
  var signer2 = document.getElementById("signer2");
  console.log(signer2);
  console.log("signing");
  signer2.SetParam('TextToBeSigned', tbs);
  console.log("from plugin: " + signer2.GetParam('TextToBeSigned'));
  signer2.SetParam('Nonce', nonce);
  console.log("from plugin: " + signer2.GetParam('Nonce'));
  signer2.SetParam('ServerTime', servertime);
  console.log("from plugin: " + signer2.GetParam('ServerTime'));
  //    signer2.SetParam('TextToBeSigned', tbs);
  //console.log("from plugin: " + signer2.GetParam('TextToBeSigned'));

  var res = signer2.PerformAction('Sign');
  if (res == 0) {
	return signer2.GetParam('Signature');
  }
  else {
	alert(res);
	return null;
  }
}

function sign2Success(transactionid, tbs, nonce, servertime, posturl, formselector) {
  console.log("transactionid: " + transactionid);
  console.log("tbs: " + tbs);
  console.log("nonce: " + nonce);
  console.log("servertime: " + servertime);
  console.log("posturl: " + posturl);
  console.log("formselector: " + formselector);
  if($.browser.msie) {
	if(hasSign2PluginIE()) {
	  installSign2IE();
	} else {
      flashBankIDMessage();
	}
  } else if($.browser.mozilla) {
	if(hasSign2PluginMozilla()){
	  installSign2Mozilla();
	} else {
	  flashBankIDMessage();
	}
  }else {
	alert("Unsupported browser.");
	return false;
  }
  var sig = doSign2(tbs, nonce, servertime);    
  if(sig) {
    displayLoadingOverlay("Verifying signature . . .");
	var form = $(formselector);
	form.find("#signatureinput").val(sig);
	form.find("#transactionidinput").val(transactionid);
    form.find("#elegprovider").val("bankid");
    form.attr("action", posturl);
	form.submit();
  } else {
	alert("could not sign");
  }
}

function flashBankIDMessage() {
  addFlashMessage("Du har inte BankID installerat. Du kan ladda ned BankID från din internetbank. Följande banker tillhandahåller BankID via internetbanken:" +
                  "<ul>" +
                  "<li>Handelsbanken</li>" +
                  "<li>SEB</li>" +
                  "<li>Swedbank</li>" +
                  "<li>SkandiaBanken</li>" +
                  "<li>Länsförsäkringar Bank</li>" +
                  "<li>Danske Bank</li>" +
                  "<li>Sparbanken Öresund</li>" +
                  "<li>Sparbanken Syd</li>" +
                  "<li>Ikano Bank</li>" +
                  "</ul>");
}

function sign2(posturl, formselector, ajaxurl) {
  var good = false;
  if($.browser.msie) {
	if(hasSign2PluginIE()) {
	  good = true;
	} else {
	  flashBankIDMessage();
	}
  } else if($.browser.mozilla) {
	if(hasSign2PluginMozilla()){
	  good = true;
	} else {
	  flashBankIDMessage();
	}
  } else {
	addFlashMessage("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  if(!good){
	return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
		  'success': function(data){
		    if(data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      sign2Success(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
		      alert("OOPS");
		    }
		    
	      },
		  error: function(){ alert("oh no!");}});
  return false;
}


function sign2Author(posturl, formselector, ajaxurl) {
  var good = false;
  if($.browser.msie) {
	if(hasSign2PluginIE()) {
	  good = true;
	} else {
	  flashBankIDMessage();
	}
  } else if($.browser.mozilla) {
	if(hasSign2PluginMozilla()){
	  good = true;
	} else {
	  flashBankIDMessage();
	}
  } else {
	addFlashMessage("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  if(!good){
	return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
          'data': {'tbs': getTBS()},
		  'success': function(data){
		    if(data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      sign2Success(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
		      alert("OOPS");
		    }
		    
	      },
		  error: function(){ alert("oh no!");}});
  return false;
}

// netid plugin

function installNetIDIE() {
  $("body").append("<OBJECT NAME='iid' id='iid' WIDTH=0 HEIGHT=0 CLASSID='CLSID:5BF56AD2-E297-416E-BC49-00B327C4426E'></OBJECT>");
}

function hasNetIDPluginIE() {
  return true;
  try { 
	// change this activexobject
	var xObj = new ActiveXObject('Nexus.AuthenticationCtl');
	if(xObj) { 
	  return true;
	} 
  } catch (e) { 
  }
  return false;
}

function hasNetIDPluginMozilla() {
  if(navigator.plugins) {
	if (navigator.plugins.length > 0) {
	  if (navigator.mimeTypes && navigator.mimeTypes["application/x-iid"]) {
		if (navigator.mimeTypes["application/x-iid"].enabledPlugin) {
		  return true;
		}
	  }
	}
  }
  return false;
}

function installNetIDMozilla() {
  $("body").append("<OBJECT NAME='iid' id='iid' WIDTH=0 HEIGHT=0 TYPE='application/x-iid'></OBJECT>");
}

function doSignNetID (tbs, nonce, servertime) {
  var signer = document.getElementById("iid");
  console.log(signer);
  console.log("signing");
  signer.SetProperty('DataToBeSigned', tbs);
  //    console.log("from plugin: " + signer.GetParam('TextToBeSigned'));
  
  var res = signer.Invoke('Sign');
  if (res == 0) {
	return signer.GetProperty('Signature');
  }
  else {
	alert(res);
	return null;
  }
}

function netIDSuccess(transactionid, tbs, nonce, servertime, posturl, formselector) {
  console.log("transactionid: " + transactionid);
  console.log("tbs: " + tbs);
  console.log("nonce: " + nonce);
  console.log("servertime: " + servertime);
  console.log("posturl: " + posturl);
  console.log("formselector: " + formselector);
  if($.browser.msie) {
	if(hasNetIDPluginIE()) {
	  installNetIDIE();
	} else {
	  flashTeliaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasNetIDPluginMozilla()){
	  installNetIDMozilla();
	} else {
	  flashTeliaMessage();
	}
  }else {
	alert("Unsupported browser.");
	return false;
  }
  var sig = doSignNetID(tbs, nonce, servertime);    
  if(sig) {
    displayLoadingOverlay("Verifying signature . . .");
	var form = $(formselector);
	form.find("#signatureinput").val(sig);
	form.find("#transactionidinput").val(transactionid);
    form.find("#elegprovider").val("telia");
    form.attr("action", posturl);
	form.submit();
  } else {
	alert("could not sign");
  }
}

function flashTeliaMessage() {
  addFlashMessage("Du har inte Telias e-legitimation installerad.");
}

function netIDSign(posturl, formselector, ajaxurl) {
  var good = false;
  if($.browser.msie) {
	if(hasNetIDPluginIE()) {
	  good = true;
	} else {
	  flashTeliaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasNetIDPluginMozilla()){
	  good = true;
	} else {
	  flashTeliaMessage();
	}
  } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  if(!good){
	return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
		  'success': function(data){
		    if(data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      netIDSuccess(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
		      alert("OOPS");
		    }
		    
	      },
		  error: function(){ alert("oh no!");}});
  return false;
}

function netIDSignAuthor(posturl, formselector, ajaxurl) {
  var good = false;
  if($.browser.msie) {
	if(hasNetIDPluginIE()) {
	  good = true;
	} else {
	  flashTeliaMessage();
	}
  } else if($.browser.mozilla) {
	if(hasNetIDPluginMozilla()){
	  good = true;
	} else {
	  flashTeliaMessage();
	}
  } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
  }
  if(!good){
	return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
          'data': {'tbs': getTBS()},
		  'success': function(data){
		    if(data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      netIDSuccess(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
		      alert("OOPS");
		    }
		    
	      },
		  error: function(){ alert("oh no!");}});
  return false;
}

safeReady(function() {
  $("button.bankid.signatory").click(function() {
    sign2("/s" + window.location.pathname.substring(2),
	      "#dialog-confirm-sign",
	      "/s/bankid" + window.location.pathname.substring(2));
    return false;
  });
  $("button.bankid.author").click(function() {
    sign2Author("/d" + window.location.pathname.substring(2),
	            "#dialog-confirm-sign-invite",
	            "/d/bankid" + window.location.pathname.substring(2));
  });
  $("button.nordea.signatory").click(function() {
    sign1("/s" + window.location.pathname.substring(2),
          "#dialog-confirm-sign",
          "/s/nordea" + window.location.pathname.substring(2));
    return false;
  });
  $("button.nordea.author").click(function() {
    sign1Author("/d" + window.location.pathname.substring(2),
                "#dialog-confirm-sign-invite",
                "/d/nordea" + window.location.pathname.substring(2));
    return false;
  });
  $("button.telia.signatory").click(function() {
    netIDSign("/s" + window.location.pathname.substring(2),
              "#dialog-confirm-sign",
              "/s/telia" + window.location.pathname.substring(2));
    return false;
  });
  $("button.telia.author").click(function() {
    netIDSignAuthor("/d" + window.location.pathname.substring(2),
                    "#dialog-confirm-sign-invite",
                    "/d/telia" + window.location.pathname.substring(2));
    return false;
  });
});

safeReady(function() {
  var elegbuttons = $("#elegbuttons");
  var signbutton = $("#dialog-confirm-signinvite a.submiter");
  $("#validationdropdown").change(function() {
    var d = $(this);
    var selected = d.find("option:selected").val();
    var numberfields = $("input[name=signatorynumber]").parents(".dragfield");
    if(selected == "email") {
      elegbuttons.hide();
      signbutton.show();
      numberfields.find(".type").html("sig");
    } else if(selected == "eleg") {
      elegbuttons.show();
      signbutton.hide();
      numberfields.find(".type").html("author");
    }
    numberfields.each(function() {
      updateStatus($(this));
    });
    
  });
});

function isAuthorSecretary() {
  return $("#authorsecretaryradio").attr("checked");
}

function getSignatoryData() {
  var entries = $(".sigentry");
  var fnames = entries.find("input[name=signatoryfstname]").map(function(i, el) {
    return el.val();
  });
  var lnames = entries.find("input[name=signatorysndname]").map(function(i, el) {
    return el.val();
  });
  var nums   = entries.find("input[name=signatorynumber]" ).map(function(i, el) {
    return el.val();
  });

  
  if(isAuthorSecretary()) {
    var authordetails = $(".authordetails");
    fnames.push(authordetails.find(".authorfstname .fieldvalue").val());
    lnames.push(authordetails.find(".authorsndname .fieldvalue").val());
    nums  .push(authordetails.find(".authornum .fieldvalue"    ).val());
  }

  var ret = [];
  fnames.each(function(i) {
    ret.push({ fname: fnames[i],
               lname: lnames[i],
               num:   nums  [i]});
  });
  return ret;
}

function generateTBS(doctitle, docid, signatories) {
  var text = 'Du undertecknar "' + doctitle + '" med transaktionsnummer "' + docid + '". Undertecknande parter är:';

  signatories.each(function() {
    text = text + "\n" + this.fname + " " + this.lname + ", " + this.num;
  });

  return text;
}

function getTBS() {
  return generateTBS($("#signStepsTitleRow .name").text()
                     ,$("#signStepsTitleRow .title").text().substring(10)
                     ,getSignatoryData());
}
