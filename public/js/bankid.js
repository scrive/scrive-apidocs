// checks for ie
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

function hasNetIDPluginIE() {
  return true;
  try { 
	var xObj = new ActiveXObject('Nexus.AuthenticationCtl');
	if(xObj) { 
	  return true;
	} 
  } catch (e) { 
  }
  return false;
}

// checks for others
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

// ie installers
function IEInstallSigner1Object() {
  $('body').append("<OBJECT ID='signerId' CLASSID='CLSID:6969E7D5-223A-4982-9B79-CC4FAC2D5E5E'> </OBJECT>");
}

function installSign2IE() {
  $("body").append('<OBJECT ID="signer2" CLASSID="CLSID:FB25B6FD-2119-4cef-A915-A056184C565E"> </OBJECT>');
}

function installNetIDIE() {
  $("body").append("<OBJECT NAME='iid' id='iid' WIDTH=0 HEIGHT=0 CLASSID='CLSID:5BF56AD2-E297-416E-BC49-00B327C4426E'> </OBJECT>");
}

//other installers
function mozillaInstallSigner1Object() {
  $('body').append('<OBJECT id="signerId" type="application/x-personal-signer" length=0 height=0> </OBJECT>');
}

function installSign2Mozilla() {
  $("body").append('<OBJECT id="signer2" type="application/x-personal-signer2" length=0 height=0></OBJECT>');
}

function installNetIDMozilla() {
  $("body").append("<OBJECT NAME='iid' id='iid' WIDTH=0 HEIGHT=0 TYPE='application/x-iid'></OBJECT>");
}

// sign
function doSign1(tbs, posturl) {
  var signer = document.getElementById('signerId');
  if(signer) {
    signer.SetDataToBeSigned(tbs);
    signer.SetIncludeCaCert('true');
    signer.SetIncludeRootCaCert('true');
    signer.SetBase64('true');
    signer.SetCharacterEncoding('UTF8');
    if (signer.Sign() == 0) {
	  return unescape(signer.GetSignature());
    } else {
      elegFail("Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör; " + signer.GetErrorString());      
    }
  } else {
    elegFail("Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör.");
  }
  return null;
}

function doSign2 (tbs, nonce, servertime) {
  var signer2 = document.getElementById("signer2");
  console.log(signer2);
  if(signer2) {
    signer2.SetParam('TextToBeSigned', tbs);
    signer2.SetParam('Nonce', nonce);
    signer2.SetParam('ServerTime', servertime);
    signer2.SetParam('TextCharacterEncoding', "UTF-8");
    var res = signer2.PerformAction('Sign');
    if (res == 0) {
	  return signer2.GetParam('Signature');
    } else {
      elegFail("Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör; error code: " + res);
    }
  } else {
    elegFail("Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör.");
  }
  return null;
}

function doSignNetID (tbs, nonce, servertime) {
  var signer = document.getElementById("iid");
  if(signer) {
    signer.SetProperty('DataToBeSigned', tbs);
    signer.SetProperty('Base64', 'true');
    signer.SetProperty('UrlEncode', 'false');
    signer.SetProperty('IncludeRootCaCert', 'true');
    signer.SetProperty('IncludeCaCert', 'true');
    var res = signer.Invoke('Sign');
    if (res == 0) {
	  return signer.GetProperty('Signature');
    } else {
      elegFail("Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör; error code: " + res);
    }
  } else {
    elegFail("Din signerings plugin misslyckades med att signera dokumentet. Var vänlig och kontakta din e-legitimationsleverantör.");
  }
  return null;
}

// success fns
function sign1Success(transactionid, tbs, nonce, servertime, posturl, formselector) {
  closeLoadingOverlay();
  if($.browser.msie && hasIESigner1Plugin()) {
	  IEInstallSigner1Object();
  } else if(hasMozillaSigner1Plugin()) {
	  mozillaInstallSigner1Object();
  } else {
	  flashNordeaMessage();
      return false;
  }
  var sig = doSign1(tbs);
  postBack(sig, "nordea", formselector, transactionid, posturl);
}

function sign2Success(transactionid, tbs, nonce, servertime, posturl, formselector) {
  closeLoadingOverlay();
  if($.browser.msie && hasSign2PluginIE()) {
	  installSign2IE();
  } else if(hasSign2PluginMozilla()) {
    installSign2Mozilla();
  } else {
	flashBankIDMessage();
    return false;
  }
  var sig = doSign2(tbs, nonce, servertime);   
  postBack(sig, "bankid", formselector, transactionid, posturl); 
}

function netIDSuccess(transactionid, tbs, nonce, servertime, posturl, formselector) {
  closeLoadingOverlay();
  if($.browser.msie && hasNetIDPluginIE()) {
	installNetIDIE();
  } else if(hasNetIDPluginMozilla()){
	installNetIDMozilla();
  } else {
	flashTeliaMessage();
    return false;
  }
  var sig = doSignNetID(tbs, nonce, servertime);    
  postBack(sig, "telia", formselector, transactionid, posturl);
}

// start the signature for signatory
function sign1() {
  if(!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/s/nordea" + url;
  var posturl = "/s" + url;
  var formselector = "#dialog-confirm-sign";
  ajaxRequest(ajaxurl, posturl, formselector, sign1Success, false);
  return false;
}

function sign2() {
  if(!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/s/bankid" + url;
  var posturl = "/s" + url;
  var formselector = "#dialog-confirm-sign";
  ajaxRequest(ajaxurl, posturl, formselector, sign2Success, false);
  return false;
}

function netIDSign() {
  if(!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/s/telia" + url;
  var posturl = "/s" + url;
  var formselector = "#dialog-confirm-sign";
  ajaxRequest(ajaxurl, posturl, formselector, netIDSuccess, false);
  return false;
}

// for author2
function sign1AwaitingAuthor() {
  if(!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/d/nordea" + url;
  var posturl = "/d" + url;
  var formselector = "#dialog-confirm-sign-by-author";
  ajaxRequest(ajaxurl, posturl, formselector, sign1Success, false);
  return false;
}

function sign2AwaitingAuthor() {
  if(!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/d/bankid" + url;
  var posturl = "/d" + url;
  var formselector = "#dialog-confirm-sign-by-author";
  ajaxRequest(ajaxurl, posturl, formselector, sign2Success, false);
  return false;
}

function netIDSignAwaitingAuthor() {
  if(!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/d/telia" + url;
  var posturl = "/d" + url;
  var formselector = "#dialog-confirm-sign-by-author";
  ajaxRequest(ajaxurl, posturl, formselector, netIDSuccess, false);
  return false;
}

// for author
function sign1Author() {
  if(!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/d/nordea" + url;
  var posturl = "/d" + url;
  var formselector = "#dialog-confirm-signinvite";
  ajaxRequest(ajaxurl, posturl, formselector, sign1Success, true);
  return false;
}

function sign2Author() {
  if(!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/d/bankid" + url;
  var posturl = "/d" + url;
  var formselector = "#dialog-confirm-signinvite";
  ajaxRequest(ajaxurl, posturl, formselector, sign2Success, true);
  return false;
}

function netIDSignAuthor() {
  if(!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage)) {
    return false;
  }
  displayLoadingOverlay("Inleder säker signering . . .");
  var url = window.location.pathname.substring(2);
  var ajaxurl = "/d/telia" + url;
  var posturl = "/d" + url;
  var formselector = "#dialog-confirm-signinvite";
  ajaxRequest(ajaxurl, posturl, formselector, netIDSuccess, true);
  return false;
}


function flashNordeaMessage() {
  addFlashMessage("Du har inte Nordeas e-legitimation installerad. Du kan ladda ned Nordeas e-legitimation från Nordeas internetbank.", "red");
}

function flashBankIDMessage() {
  addFlashMessage("Du har inte BankID installerat. Du kan ladda ned BankID från din internetbank. Följande banker tillhandahåller BankID via internetbanken.", "red");
}

function flashTeliaMessage() {
  addFlashMessage("Du har inte Telias e-legitimation installerad.", "red");
}


function failEleg(msg) {
  addFlashMessage(msg, "red");
  closeLoadingOverlay();
}

safeReady(function() {
  $("a.bankid.signatory").click(sign2);
  $("a.bankid.author").click(sign2Author);
  $("a.bankid.author2").click(sign2AwaitingAuthor);

  $("a.nordea.signatory").click(sign1);
  $("a.nordea.author").click(sign1Author);
  $("a.nordea.author2").click(sign1AwaitingAuthor);

  $("a.telia.signatory").click(netIDSign);
  $("a.telia.author").click(netIDSignAuthor);
  $("a.telia.author2").click(netIDSignAwaitingAuthor);
});

safeReady(function() {
  var eleghidden = $(".eleghidden");
  var signhidden = $(".signhidden");
  $("#validationdropdown").change(function() {
    var d = $(this);
    var selected = d.find("option:selected").val();
    var numberfields = $("input[name=signatorynumber]").parents(".dragfield");
    if(selected == "email") {
      eleghidden.hide();
      signhidden.show();
      numberfields.find(".type").html("sig");
    } else if(selected == "eleg") {
      eleghidden.show();
      signhidden.hide();
      numberfields.find(".type").html("author");
    }
    numberfields.each(function() {
      updateStatus($(this));
    });
  }).change();
});

function isAuthorSecretary() {
  return $("#authorsecretaryradio").attr("checked");
}

function getSignatoryData() {
  var entries = $("#personpane .sigentry");
  var fnames = entries.find("input[name=signatoryfstname]").map(function(i, el) {
    return $(el).val();
  });
  var lnames = entries.find("input[name=signatorysndname]").map(function(i, el) {
    return $(el).val();
  });
  var nums   = entries.find("input[name=signatorynumber]" ).map(function(i, el) {
    return $(el).val();
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

  if($("a.group").size() > 0) {
    text = text + "\n   Various signatories.";
  } else {
    $(signatories).each(function() {
      text = text + "\n" + this.fname + " " + this.lname + ", " + this.num;
    });
  }

  return text;
}

function getTBS() {
  return generateTBS($("#signStepsTitleRow .name").text()
                     ,$("#signStepsTitleRow .title").text().substring(10)
                     ,getSignatoryData());
}

function checkPlugin(iefn, otfn, msgfn) {
  if(($.browser.msie && iefn()) || otfn()) {
    return true;
  } else {
    msgfn();
    return false;
  }
}

function ajaxRequest(ajaxurl, posturl, formselector, successfn, tbs) {
  $.ajax({'url': ajaxurl,
		  'dataType': 'json',
          'data': tbs?{'tbs': getTBS()}:{},
          'scriptCharset': "utf-8",
		  'success': function(data){
		    if(data && data['status'] === 0) {
		      var tbs = data['tbs'];
		      var nonce = data['nonce'];
		      var servertime = data['servertime'];
		      var transactionid = data['transactionid'];
		      successfn(transactionid, tbs, nonce, servertime, posturl, formselector);
		    } else {
              failEleg(data.msg);
		    }
	      },
		  error: repeatForeverWithDelay(250)});
}

function postBack(sig, provider, formselector, transactionid, posturl) {
  if(sig) {
    displayLoadingOverlay("Verifierar signatur...");
	var form = $(formselector);
    form.find("#signauto").attr("name", "dontsign");
	form.find("#signatureinput").val(sig);
	form.find("#transactionidinput").val(transactionid);
    form.find("#elegprovider").val(provider);
    form.find("#elegprovider").attr("name", "eleg");
    form.attr("action", posturl);
	form.submit();
  }
}
