var tbstext = 'I think I want to sign this.';

// authentication

function installIEAuthenticationObject(){
    $("body").append("<OBJECT ID='authenticate' CLASSID='CLSID:DD137900-E4D7-4b86-92CC-2E968F846047'> </OBJECT>");
}

function hasIEAuthenticationPlugin(){
    try { 
	var xObj = new ActiveXObject('Nexus.AuthenticationCtl');
	if(xObj) { 
	    return true;
	} 
    } catch (e) { 
    }
    return false;
}

function IEAuthenticate(challenge) {
    authenticate.SetParam('Challenge',challenge); 
    authenticate.SetParam('ServerTime', (String)(Math.round(new Date().getTime() / 1000))); 
    var res = authenticate.PerformAction('Authenticate'); 
    if (res == 0) { 
	return authenticate.GetParam('Signature');
    } else { 
	console.log("Failed to authenticate. Error: " + res);
    }
    return null;
}

function installMozillaAuthenticateObject() {
    $("body").append("<OBJECT id='authenticate' type='application/x-personal-authentication' length=0 height=0> </OBJECT>");
}

function hasMozillaAuthenticationPlugin() {
    if(navigator.plugins && navigator.plugins.length > 0) { 
	if (navigator.mimeTypes) {
	    if(navigator.mimeTypes['application/x-personal-authentication']) { 
		if (navigator.mimeTypes['application/x-personal-authentication'].enabledPlugin) { 
		    return true;
		}
	    }
	}
    }
    return false;
}

function mozillaAuthenticate(challenge) {
    authenticate.SetParam('Challenge', challenge);
    authenticate.SetParam('ServerTime',(String)(Math.round(new Date().getTime()/1000)));
    var res = authenticate.PerformAction('Authenticate'); 
    if (res == 0) {
	return authenticate.GetParam('Signature');
    } else {
	console.log('Failed to create authentication signature. Error = '+res);
    } 
    return null;
}

function canBankIDAuthenticate() {
    if($.browser.msie) {
	return hasIEAuthenticationPlugin();
    } else if($.browser.mozilla) {
	return hasMozillaAuthenticationPlugin();
    }
    return false;
}

function bankidAuthenticate(challenge) {
    if($.browser.msie) {
	if(hasIEAuthenticationPlugin()){
	    installIEAuthenticateObject();
	    return IEAuthenticate(challenge);
	} else {
	    alert("No plugin");
	}
    } else if($.browser.mozilla) {
	if(hasMozillaAuthenticationPlugin()) {
	    installMozillaAuthenticateObject();
	    return mozillaAuthenticate(challenge);
	} else {
	    alert("No plugin");
	}
    } else {
	alert("Unsupported browser");
    }
    return null;
}

function bankIDLoginSuccess(challenge) {
    var sig = bankidAuthenticate(challenge);
    if(sig) {
	// challenge should come from the server-side session
	$('body').append("<form id='bankidlogin' method='post' action='/bankidlogin'><input type='hidden' name='signature' value='" + sig + "' /><input type='hidden' name='challenge' value='"+challenge+"' /></form>");
	$('#bankidlogin').submit();
    }
}

function bankIDLogin() {
    var good = false;
    if($.browser.msie) {
	if(hasIEAuthenticationPlugin()) {
	    good = true;
	} else {
	    alert("Please install the BankID Plugin");
	}
    } else if($.browser.mozilla) {
	if(hasMozillaAuthenticationPlugin()){
	    good = true;
	} else {
	    alert("Please install the BankID Plugin");
	}
    } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
    }
    if(!good){
	return false;
    }
    $.ajax({'url': '/bankid-challenge',
		'success': bankIDLoginSuccess});
}

// signing

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

function IESign1(tbs) {
    signer.SetMimeType('text/plain');
    signer.SetCharacterEncoding('platform');
    signer.SetFormat('PKCS7SIGNED_Attached');
    signer.SetFileName('skrivapa.txt');
    signer.SetWindowName('_self'); 
    signer.SetDataToBeSigned(tbs); 
    signer.SetSignReturnName('SignedData'); 
    signer.SetDataReturnName('UnsignedData'); 
    signer.SetVersionReturnName('Version'); 
    signer.SetIssuers(''); 
    signer.SetPostURL('/bankidsign');
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

function mozillaSign1(tbs) {
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
    signer.SetPostURL('/bankidsign');  // automatically post it
    signer.SetIncludeCaCert('true');
    signer.SetIncludeRootCaCert('true');
    if (signer.Sign() == 0) {
	return signer.GetSignature();
    } else {
	console.log(signer.GetErrorString());
    }
    return null;
}



function sign1Success(data) {
    var tbs = data;
    var sig = doSign(tbs);    
    if(sig) {
	// tbs should actually come from the server-side session
	// DO NOT DEPLOY WITHOUT CHANGING
	//$('body').append("<form id='bankidsign' method='post' action='/bankidsign'><input type='hidden' name='SignedData' id='sigfield' value='" + sig + "' /><input type='hidden' name='tbs' value='"+tbs+"' /></form>");
	//$('#bankidsign').submit();
    }
}

function doSign1(tbs) {
    if($.browser.msie) {
	if(hasIESigner1Plugin()) {
	    IEInstallSigner1Object();
	    return IESign1(tbs);
	} else {
	    alert("No plugin");
	}
    } else if($.browser.mozilla) {
	if(hasMozillaSigner1Plugin()){
	    mozillaInstallSigner1Object();
	    return mozillaSign1(tbs);
	} else {
	    alert("No plugin");
	}
    } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
    }
    return null;
}

function sign1() {
    var good = false;
    if($.browser.msie) {
	if(hasIESigner1Plugin()) {
	    good = true;
	} else {
	    alert("Please install the BankID Plugin");
	}
    } else if($.browser.mozilla) {
	if(hasMozillaSigner1Plugin()){
	    good = true;
	} else {
	    alert("Please install the BankID Plugin");
	}
    } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
    }
    if(!good){
	return false;
    }
    $.ajax({'url': '/bankid-encodetbs', 
		'data': {'tbs': tbstext},
		'success': sign1Success});
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
	    alert("Please install the BankID Plugin");
	}
    } else if($.browser.mozilla) {
	if(hasSign2PluginMozilla()){
	    installSign2Mozilla();
	} else {
	    alert("Please install the BankID Plugin");
	}
    }else {
	alert("Unsupported browser.");
	return false;
    }
    var sig = doSign2(tbs, nonce, servertime);    
    if(sig) {
	var form = $(formselector);
	form.find("#signature").val(sig);
	form.find("#transactionid").val(transactionid);
	form.submit();
    } else {
	alert("could not sign");
    }
}

function sign2(posturl, formselector, ajaxurl) {
    var good = false;
    if($.browser.msie) {
	if(hasSign2PluginIE()) {
	    good = true;
	} else {
	    alert("Please install the BankID Plugin");
	}
    } else if($.browser.mozilla) {
	if(hasSign2PluginMozilla()){
	    good = true;
	} else {
	    alert("Please install the BankID Plugin");
	}
    } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
    }
    if(!good){
	return false;
    }
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
	    alert("Please install the NetID Plugin");
	}
    } else if($.browser.mozilla) {
	if(hasNetIDPluginMozilla()){
	    installNetIDMozilla();
	} else {
	    alert("Please install the NetID Plugin");
	}
    }else {
	alert("Unsupported browser.");
	return false;
    }
    var sig = doSignNetID(tbs, nonce, servertime);    
    if(sig) {
	var form = $(formselector);
	form.find("#signature").val(sig);
	form.find("#transactionid").val(transactionid);
	form.submit();
    } else {
	alert("could not sign");
    }
}


function netIDSign(posturl, formselector, ajaxurl) {
    var good = false;
    if($.browser.msie) {
	if(hasNetIDPluginIE()) {
	    good = true;
	} else {
	    alert("Please install the NetID Plugin");
	}
    } else if($.browser.mozilla) {
	if(hasNetIDPluginMozilla()){
	    good = true;
	} else {
	    alert("Please install the NetID Plugin");
	}
    } else {
	alert("Your browser is not supported. Please use either Internet Explorer or Firefox.");
    }
    if(!good){
	return false;
    }
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
}

