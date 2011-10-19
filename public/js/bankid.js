// checks for ie
function hasIESigner1Plugin() {
    try {
        return !!(new ActiveXObject(Nexus.SignerCtl));
    } catch(e) {
        return false;
    }
}

function hasSign2PluginIE() {
    try {
        return !!(new ActiveXObject("Nexus.SignerV2Ctl"));
    } catch(e) {
        return false;
    }
}

function hasNetIDPluginIE() {
    // This function always returns true. The detection does not work.
    // Eric
    return true;
    /**
    try {
        return !!(new ActiveXObject('Nexus.AuthenticationCtl'));
    } catch(e) {
        return false;
    }*/
}

// checks for others
function hasMozillaSigner1Plugin() {
    return (navigator.plugins 
            && navigator.plugins.length > 0
            && navigator.mimeTypes && navigator.mimeTypes["application/x-personal-signer"]
            && navigator.mimeTypes["application/x-personal-signer"].enabledPlugin);
}

function hasSign2PluginMozilla() {
    return (navigator.plugins
            && navigator.plugins.length > 0
            && navigator.mimeTypes && navigator.mimeTypes["application/x-personal-signer2"]
            && navigator.mimeTypes["application/x-personal-signer2"].enabledPlugin);
}

function hasNetIDPluginMozilla() {
    return (navigator.plugins
            && navigator.plugins.length > 0
            && navigator.mimeTypes && navigator.mimeTypes["application/x-iid"]
            && navigator.mimeTypes["application/x-iid"].enabledPlugin);
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
function doSign1(tbs) {
    var signer = document.getElementById('signerId');
    if(!signer) // could not find element means plugin activation failed
        return failEleg(localization.yourSigningPluginFailed);

    signer.SetDataToBeSigned(tbs);
    signer.SetIncludeCaCert('true');
    signer.SetIncludeRootCaCert('true');
    signer.SetBase64('true');
    signer.SetCharacterEncoding('UTF8');
    signer.SetMimeType('text/plain;charset=UTF-8');
    signer.SetViewData('false');

    if (signer.Sign() === 0) // 0 means success
        return unescape(signer.GetSignature());
    else
        return failEleg(localization.yourSigningPluginFailed + " " + signer.GetErrorString());
}

function doSign2(tbs, nonce, servertime) {
    var signer2 = document.getElementById("signer2");
    if(!signer2) // plugin installation failed
        return failEleg(localization.yourSigningPluginFailed);

    signer2.SetParam('TextToBeSigned', tbs);
    signer2.SetParam('Nonce', nonce);
    signer2.SetParam('ServerTime', servertime);
    signer2.SetParam('TextCharacterEncoding', "UTF-8");
    var res = signer2.PerformAction('Sign');

    if (res === 0) // 0 means success
        return signer2.GetParam('Signature');
    else
        return failEleg(localization.yourSigningPluginFailed + " error code: " + res);
}

function doSignNetID(tbs, nonce, servertime) {
    var signer = document.getElementById("iid");
    if(!signer) // installation failed
        return failEleg(localization.yourSigningPluginFailed);

    signer.SetProperty('DataToBeSigned', tbs);
    signer.SetProperty('Base64', 'true');
    signer.SetProperty('UrlEncode', 'false');
    signer.SetProperty('IncludeRootCaCert', 'true');
    signer.SetProperty('IncludeCaCert', 'true');
    var res = signer.Invoke('Sign');
    if (res === 0) // 0 means success
        return signer.GetProperty('Signature');
    else
        return failEleg(localization.yourSigningPluginFailed + " error code: " + res);
}

// success fns
function sign1Success(transactionid, tbs, nonce, servertime, posturl, formselector) {
    closeLoadingOverlay(); // this was opened just before starting
    // ajax request
    if ($.browser.msie && hasIESigner1Plugin())
        IEInstallSigner1Object();
    else if (hasMozillaSigner1Plugin())
        mozillaInstallSigner1Object();
    else
        return flashNordeaMessage();
    postBack(doSign1(tbs), "nordea", formselector, transactionid, posturl);
}

function sign2Success(transactionid, tbs, nonce, servertime, posturl, formselector) {
    closeLoadingOverlay();
    if ($.browser.msie && hasSign2PluginIE())
        installSign2IE();
    else if (hasSign2PluginMozilla())
        installSign2Mozilla();
    else
        return flashBankIDMessage();
    postBack(doSign2(tbs, nonce, servertime), "bankid", formselector, transactionid, posturl);
}

function netIDSuccess(transactionid, tbs, nonce, servertime, posturl, formselector) {
    closeLoadingOverlay();
    if ($.browser.msie && hasNetIDPluginIE())
        installNetIDIE();
    else if (hasNetIDPluginMozilla())
        installNetIDMozilla();
    else
        return flashTeliaMessage();
    postBack(doSignNetID(tbs, nonce, servertime), "telia", formselector, transactionid, posturl);
}

// start the signature for signatory
function sign1() {
    if (!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/s/nordea" + url;
    var posturl = "/s" + url;
    var formselector = "#dialog-confirm-sign";
    ajaxRequest(ajaxurl, posturl, formselector, sign1Success, false);
    return false;
}

function sign2() {
    if (!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/s/bankid" + url;
    var posturl = "/s" + url;
    var formselector = "#dialog-confirm-sign";
    ajaxRequest(ajaxurl, posturl, formselector, sign2Success, false);
    return false;
}

function netIDSign() {
    if (!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/s/telia" + url;
    var posturl = "/s" + url;
    var formselector = "#dialog-confirm-sign";
    ajaxRequest(ajaxurl, posturl, formselector, netIDSuccess, false);
    return false;
}

// for author2
function sign1AwaitingAuthor() {
    if (!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/d/nordea" + url;
    var posturl = "/d" + url;
    var formselector = "#dialog-confirm-sign-by-author";
    ajaxRequest(ajaxurl, posturl, formselector, sign1Success, false);
    return false;
}

function sign2AwaitingAuthor() {
    if (!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/d/bankid" + url;
    var posturl = "/d" + url;
    var formselector = "#dialog-confirm-sign-by-author";
    ajaxRequest(ajaxurl, posturl, formselector, sign2Success, false);
    return false;
}

function netIDSignAwaitingAuthor() {
    if (!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/d/telia" + url;
    var posturl = "/d" + url;
    var formselector = "#dialog-confirm-sign-by-author";
    ajaxRequest(ajaxurl, posturl, formselector, netIDSuccess, false);
    return false;
}

// for author
function sign1Author() {
    if (!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/d/nordea" + url;
    var posturl = "/d" + url;
    var formselector = "#dialog-confirm-signinvite";
    ajaxRequest(ajaxurl, posturl, formselector, sign1Success, true);
    return false;
}

function sign2Author() {
    if (!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/d/bankid" + url;
    var posturl = "/d" + url;
    var formselector = "#dialog-confirm-signinvite";
    ajaxRequest(ajaxurl, posturl, formselector, sign2Success, true);
    return false;
}

function netIDSignAuthor() {
    if (!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage))
        return false;

    displayLoadingOverlay(localization.startingSaveSigning);
    var url = window.location.pathname.substring(2);
    var ajaxurl = "/d/telia" + url;
    var posturl = "/d" + url;
    var formselector = "#dialog-confirm-signinvite";
    ajaxRequest(ajaxurl, posturl, formselector, netIDSuccess, true);
    return false;
}

function flashNordeaMessage() {
    FlashMessages.add({ content: localization.noNordeaInstalled, color: "red"});
    return false;
}

function flashBankIDMessage() {
    FlashMessages.add({ content: localization.noBankIdInstalled, color: "red"});
    return false;
}

function flashTeliaMessage() {
    FlashMessages.add({ content: localization.noTeliaInstalled, color: "red"});
    return false;
}

function failEleg(msg) {
    FlashMessages.add({ content: msg, color: "red"});
    closeLoadingOverlay();
    return null;
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

// set up the sign modals when eleg/email is selected
safeReady(function() {
    var eleghidden = $(".eleghidden");
    var signhidden = $(".signhidden");
    $("#validationdropdown").change(function() {
        var d = $(this);
        var selected = d.find("option:selected").val();
        var numberfields = $("input[name=signatorypersonalnumber]").parents(".dragfield");
        if (selected == "email") {
            eleghidden.hide();
            signhidden.show();
            numberfields.find(".type").html("sig");
        } else if (selected == "eleg") {
            eleghidden.show();
            signhidden.hide();
            numberfields.find(".type").html("author");
        }
        numberfields.each(function() {
            updateStatus($(this));
        });
    }).change();
});

// I hope this gets subsumed into backbone.js
function isAuthorSecretary() {
    return $("#authorsecretaryradio").attr("checked");
}

// build a js object containing first name, last name, and personal
// number for all signatories
// I hope this gets subsumed into backbone.js
function getSignatoryData() {
    var entries = $("#personpane .sigentry");
    var fnames = entries.find("input[name=signatoryfstname]").map(function(i, el) {
        return $(el).val();
    });
    var lnames = entries.find("input[name=signatorysndname]").map(function(i, el) {
        return $(el).val();
    });
    var nums = entries.find("input[name=signatorypersonalnumber]").map(function(i, el) {
        return $(el).val();
    });

    if (!isAuthorSecretary()) {
        var authordetails = $(".authordetails");
        var fn = authordetails.find(".authorfstname .fieldvalue");
        var ln = authordetails.find(".authorsndname .fieldvalue");
        var nm = authordetails.find(".authorpersnum .fieldvalue");
        fnames.push(fn.size() > 0 ? fn.text() : "no first name");
        lnames.push(ln.size() > 0 ? ln.text() : "no last name");
        nums.push(nm.size() > 0 ? nm.text() : "no personnnummer");
    }

    var ret = [];
    fnames.each(function(i) {
        ret.push({
            fname: fnames[i],
            lname: lnames[i],
            num: nums[i]
            });
    });
    return ret;
}

// generate a TBS from the available data
function generateTBS(doctitle, docid, signatories) {
    var text = localization.tbsGenerationMessage(doctitle, docid);

    if($("a.group").size() > 0)
        return text + "\n " + localization.differentSignatories;
    else
        $(signatories).each(function() {
            text = text + "\n" + this.fname + " " + this.lname + ", " + this.num;
        });
    return text;
}

function getTBS() {
    return generateTBS($("#signStepsTitleRow .name").text(), $("#signStepsTitleRow .title").text().substring(10), getSignatoryData());
}

function checkPlugin(iefn, otfn, msgfn) {
    if (($.browser.msie && iefn()) || otfn())
        return true;
    else
        msgfn();
    return false;
}

function ajaxRequest(ajaxurl, posturl, formselector, successfn, tbs) {
    $.ajax({
        'url': ajaxurl,
        'dataType': 'json',
        'data': tbs ? { 'tbs': getTBS() }: {},
        'scriptCharset': "utf-8",
        'success': function(data) {
            if (data && data.status === 0)
                successfn(data.transactionid, data.tbs, data.nonce, data.servertime, posturl, formselector);
            else
                failEleg(data.msg);
        },
        error: repeatForeverWithDelay(250)
    });
}

function postBack(sig, provider, formselector, transactionid, posturl) {
    if (!sig)
        return false;

    displayLoadingOverlay(localization.verifyingSignature);
    var form = $(formselector);
    form.find("#signauto").attr("name", "dontsign");
    form.find("#signatureinput").val(sig);
    form.find("#transactionidinput").val(transactionid);
    form.find("#elegprovider").val(provider);
    form.find("#elegprovider").attr("name", "eleg");
    form.attr("action", posturl);
    form.submit();
}


/* Totally new functions set for backbone connected stuff */

(function(window){

window.Eleg = {
    bankidSign : function(document, signatory, submit) {
      if (!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage))
        return false;
      LoadingDialog.open(localization.startingSaveSigning);
      $.ajax({
            'url': "/s/bankid/" + document.documentid() +  document.viewer().urlPart(),
            'dataType': 'json',
            'data': {}, 
            'scriptCharset': "utf-8",
            'success': function(data) {
              if (data && data.status === 0)  {
               closeLoadingOverlay(); // this was opened just before starting
                if ($.browser.msie && hasSign2PluginIE())
                    installSign2IE();
                else if (hasSign2PluginMozilla())
                    installSign2Mozilla();
                else {
                    flashBankIDMessage();
                    return; }
                var signer = $('#signer2')[0];
                if(!signer)  {
                     FlashMessages.add({ content: localization.yourSigningPluginFailed, color: "red"});
                     LoadingDialog.close();
                     failEleg(localization.yourSigningPluginFailed);
                     return;
                }
               signer.SetParam('TextToBeSigned', data.tbs);
               signer.SetParam('Nonce', data.nonce);
               signer.SetParam('ServerTime', data.servertime);
               signer.SetParam('TextCharacterEncoding', "UTF-8");
               var res = signer.PerformAction('Sign');
               if (res !== 0) // 0 means success
                {
                    FlashMessages.add({ content: localization.yourSigningPluginFailed + " " + res, color: "red"});
                    LoadingDialog.close();
                    return;
                }
                var signresult =  signer.GetParam('Signature');
                if (!signresult)
                    return;
                LoadingDialog.open(localization.verifyingSignature);
                submit.add("signature",signresult);
                submit.add("transactionid", data.transactionid);
                submit.add("eleg" , "bankid");
                submit.send();
            }    
            else
                FlashMessages.add({ content: data.msg, color: "red"});
            LoadingDialog.close();
            },
            error: repeatForeverWithDelay(250)
      });  
    },
    nordeaSign : function(document, signatory, submit) {
       if (!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage))
            return;

    LoadingDialog.open(localization.startingSaveSigning);
    $.ajax({
            'url': "/s/nordea/" + document.documentid() +  document.viewer().urlPart(),
            'dataType': 'json',
            'data': {}, 
            'scriptCharset': "utf-8",
            'success': function(data) {
              if (data && data.status === 0)  {
               closeLoadingOverlay(); // this was opened just before starting
                if ($.browser.msie && hasIESigner1Plugin())
                    IEInstallSigner1Object();
                else if (hasMozillaSigner1Plugin())
                    mozillaInstallSigner1Object();
                else {
                    flashNordeaMessage();
                    return; }
                var signer = $('#signerId')[0];
                if(!signer)  {
                     FlashMessages.add({ content: localization.yourSigningPluginFailed, color: "red"});
                     LoadingDialog.close();
                     failEleg(localization.yourSigningPluginFailed);
                     return;
                }
                signer.SetDataToBeSigned(data.tbs);
                signer.SetIncludeCaCert('true');
                signer.SetIncludeRootCaCert('true');
                signer.SetBase64('true');
                signer.SetCharacterEncoding('UTF8');
                signer.SetMimeType('text/plain;charset=UTF-8');
                signer.SetViewData('false');
                var res = signer.Sign();
                if (res !== 0) // 0 means success
                {
                    FlashMessages.add({ content: localization.yourSigningPluginFailed + " " + signer.GetErrorString(), color: "red"});
                    LoadingDialog.close();
                    return;
                }
                var signresult =  unescape(signer.GetSignature());
                if (!signresult)
                    return;
                LoadingDialog.open(localization.verifyingSignature);
                submit.add("signature",signresult);
                submit.add("transactionid", data.transactionid);
                submit.add("eleg" , "nordea");
                submit.send();
            }    
            else
                FlashMessages.add({ content: data.msg, color: "red"});
            LoadingDialog.close();
            },
            error: repeatForeverWithDelay(250)
      });
    
    
    
    },
    teliaSign : function(document, signatory, submit) {
        if (!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage))
            return false;
        LoadingDialog.open(localization.startingSaveSigning);
        $.ajax({
            'url': "/s/telia/" + document.documentid() +  document.viewer().urlPart(),
            'dataType': 'json',
            'data': {}, 
            'scriptCharset': "utf-8",
            'success': function(data) {
            if (data && data.status === 0)  {
                LoadingDialog.close();
                if ($.browser.msie && hasNetIDPluginIE())
                     installNetIDIE();
                else if (hasNetIDPluginMozilla())
                     installNetIDMozilla();
                else {
                     flashTeliaMessage();
                     return; }
                var signer = $("#iid")[0];
                if(!signer) {
                     FlashMessages.add({ content: localization.yourSigningPluginFailed, color: "red"});
                     LoadingDialog.close();
                     failEleg(localization.yourSigningPluginFailed);
                     return;
                }

                signer.SetProperty('DataToBeSigned', data.tbs);
                signer.SetProperty('Base64', 'true');
                signer.SetProperty('UrlEncode', 'false');
                signer.SetProperty('IncludeRootCaCert', 'true');
                signer.SetProperty('IncludeCaCert', 'true');
                var res = signer.Invoke('Sign');
                if (res !== 0) {
                    FlashMessages.add({ content: localization.yourSigningPluginFailed + " error code: " + res, color: "red"});
                    LoadingDialog.close();
                    return;
                }
                var signresult =  signer.GetProperty('Signature');
                if (!signresult)
                    return;
                LoadingDialog.open(localization.verifyingSignature);
                submit.add("signature",signresult);
                submit.add("transactionid", data.transactionid);
                submit.add("eleg" , "telia");
                submit.send();

            }    
            else
                FlashMessages.add({ content: data.msg, color: "red"});
            LoadingDialog.close();
            
            
        },
        error: repeatForeverWithDelay(250)
            
    });
}    

};
    
})(window);
