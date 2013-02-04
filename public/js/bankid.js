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
    try {
        return !!(new ActiveXObject('IID.iIDCtl'));
    } catch(e) {
        return false;
    }
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

function flashNordeaMessage() {
    new FlashMessage({ content: localization.noNordeaInstalled, color: "red"});
    return false;
}

function flashBankIDMessage() {
    new FlashMessage({ content: localization.noBankIdInstalled, color: "red"});
    return false;
}

function flashTeliaMessage() {
    new FlashMessage({ content: localization.noTeliaInstalled, color: "red"});
    return false;
}

function failEleg(msg, personalNumber) {
    if( personalNumber!=undefined && personalNumber!="" && personalNumber!=null ) {
        msg = msg + " (" + personalNumber + ")";
    }
    new FlashMessage({ content: msg, color: "red"});
    LoadingDialog.close();
    return null;
}

function checkPlugin(iefn, otfn, msgfn) {
    if (($.browser.msie && iefn()) || otfn())
        return true;
    else
        msgfn();
    return false;
}

/* Totally new functions set for backbone connected stuff */

(function(window){


window.Eleg = {
  // generate a TBS from the available data
   generateTBS : function(doctitle, docid, signatories) {
     var text = localization.tbsGenerationMessage(doctitle, docid);
     $(signatories).each(function() {
         text = text + "\n" + this.fstname() + " " + this.sndname() + ", " + this.personalnumber();
     });
     return text;
   },
   bankidSign : function(document, signatory, submit, callback) {
      if (!checkPlugin(hasSign2PluginIE, hasSign2PluginMozilla, flashBankIDMessage))
        return false;
      LoadingDialog.open(localization.startingSaveSigning);

      var url;
      if(document.preparation()) // designview
        url = "/d/eleg/" + document.documentid();
      else
        url = "/s/eleg/" + document.documentid() +  "/" + document.viewer().signatoryid();
      var tbs = window.Eleg.generateTBS(document.title(), document.documentid(), document.signatories());
      $.ajax({
            'url': url,
            'dataType': 'json',
            'data': { 'provider' : 'bankid',
                      'tbs' : tbs
                    },
            'scriptCharset': "utf-8",
            'success': function(data) {
              if (data && data.status === 0)  {
	       console.log("before");
	       console.log(data.tbs);
 	       console.log("after");
               LoadingDialog.close(); // this was opened just before starting
                if ($.browser.msie && hasSign2PluginIE())
                    installSign2IE();
                else if (hasSign2PluginMozilla())
                    installSign2Mozilla();
                else {
                    flashBankIDMessage();
                    return; }
                var signer = $('#signer2')[0];
                if(!signer)  {
                     new FlashMessage({ content: localization.yourSigningPluginFailed, color: "red"});
                     LoadingDialog.close();
                     failEleg(localization.yourSigningPluginFailed);
                     return;
                }
               signer.SetParam('TextToBeSigned', data.tbs);
               signer.SetParam('Nonce', data.nonce);
               signer.SetParam('ServerTime', data.servertime);
               //signer.SetParam('TextCharacterEncoding', "UTF-8");
               var res = signer.PerformAction('Sign');
               if (res !== 0) // 0 means success
                {
                    new FlashMessage({ content: localization.yourSigningPluginFailed + " " + res, color: "red"});
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
                if (callback == undefined)
                    submit.send();
                else
                    callback(submit);
            }
            else
               new FlashMessage({ content: data.msg, color: "red"});
            LoadingDialog.close();
            },
            error: repeatForeverWithDelay(250)
      });
    },
    nordeaSign : function(document, signatory, submit, callback) {
      if (!checkPlugin(hasIESigner1Plugin, hasMozillaSigner1Plugin, flashNordeaMessage))
        return;
      var url;
      if(document.preparation()) // // designview
        url = "/d/eleg/" + document.documentid();
      else
        url = "/s/eleg/" + document.documentid() +  "/" + document.viewer().signatoryid();
      var tbs = window.Eleg.generateTBS(document.title(), document.documentid(), document.signatories());
      LoadingDialog.open(localization.startingSaveSigning);
    $.ajax({
            'url': url,
            'dataType': 'json',
            'data': {  'provider' : 'nordea' ,
                       'tbs' : tbs
                    },

            'scriptCharset': "utf-8",
            'success': function(data) {
              if (data && data.status === 0)  {
               LoadingDialog.close(); // this was opened just before starting
                if ($.browser.msie && hasIESigner1Plugin())
                    IEInstallSigner1Object();
                else if (hasMozillaSigner1Plugin())
                    mozillaInstallSigner1Object();
                else {
                    flashNordeaMessage();
                    return; }
                var signer = $('#signerId')[0];
                if(!signer)  {
                     new FlashMessage({ content: localization.yourSigningPluginFailed, color: "red"});
                     LoadingDialog.close();
                     failEleg(localization.yourSigningPluginFailed);
                     return;
                }
                signer.SetDataToBeSigned(data.tbs);
                signer.SetIncludeCaCert('true');
                signer.SetIncludeRootCaCert('true');
                signer.SetBase64('true');
                signer.SetCharacterEncoding('UTF8');
                signer.SetMimeType('text/plain');
                signer.SetViewData('false');
                var res = signer.Sign();
                if (res !== 0) // 0 means success
                {
                    new FlashMessage({ content: localization.yourSigningPluginFailed + " " + signer.GetErrorString(), color: "red"});
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
                if (callback == undefined)
                    submit.send();
                else
                    callback(submit);
            }
            else
                new FlashMessage({ content: data.msg, color: "red"});
            LoadingDialog.close();
            },
            error: repeatForeverWithDelay(250)
      });



    },
    teliaSign : function(document, signatory, submit, callback) {
      if (!checkPlugin(hasNetIDPluginIE, hasNetIDPluginMozilla, flashTeliaMessage))
        return false;
      var url;
      if(document.preparation()) // designview
        url = "/d/eleg/" + document.documentid();
      else
        url = "/s/eleg/" + document.documentid() +  "/" + document.viewer().signatoryid();
      var tbs = window.Eleg.generateTBS(document.title(), document.documentid(), document.signatories());
        LoadingDialog.open(localization.startingSaveSigning);
        $.ajax({
            'url': url,
            'dataType': 'json',
            'data': { 'provider' : 'telia',
                      'tbs' : tbs
                    },
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
                     new FlashMessage({ content: localization.yourSigningPluginFailed, color: "red"});
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
                    new FlashMessage({ content: localization.yourSigningPluginFailed + " error code: " + res, color: "red"});
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
                if (callback == undefined)
                    submit.send();
                else
                    callback(submit);

            }
            else
               new FlashMessage({ content: data.msg, color: "red"});
            LoadingDialog.close();


        },
        error: repeatForeverWithDelay(250)

    });
    },
    mobileBankIDSign: function(document, signatory, submit, callback, personnummer) {
        var eleg = this;
        var url;
        if(document.preparation())// designview
            url = "/d/eleg/mbi/" + document.documentid();
        else
            url = "/s/eleg/mbi/" + document.documentid() +  "/" + document.viewer().signatoryid();
        console.log(url);
        LoadingDialog.open(localization.sign.eleg.mobile.startingMobileBankID);
        var fetching = true;

        var data = {};
        if(personnummer)
            data.personnummer = personnummer;
        $.ajax({
            'url': url,
            'dataType': 'json',
            'data': data,
            'type': 'POST',
            'scriptCharset': "utf-8",
            'success': function(data) {
                fetching = false;
                if (data && !data.error)  {
                    LoadingDialog.open(data.msg);
                } else if (data && data.error) {
                    new FlashMessage({ content: data.error, color: "red"});
                    LoadingDialog.close();
                    return;
                }
                var m = new MobileBankIDPolling({docid: document.documentid()
                                                 , collecturl:url
                                                 ,trid: data.transactionid
                                                 ,slid: document.viewer().signatoryid()
                                                 ,callback: function() {
                                                     submit.add("transactionid", data.transactionid);
                                                     submit.add("eleg" , "mobilebankid");
                                                     if (!callback)
                                                         submit.send();
                                                     else
                                                         callback(submit);
                                                 }
                                                });
                var mv = new MobileBankIDPollingView({model:m});
                m.poll();

            }});
        // retry after 5 seconds if it hasn't worked.
        window.setTimeout(function() {if (fetching) eleg.mobileBankIDSign(document,signatory,submit,callback);}, 5000);
    }

};

})(window);
