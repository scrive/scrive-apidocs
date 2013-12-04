/*
 * API demo main model + view
 */


(function(window) {

window.ApiCall = Backbone.Model.extend({
        defaults: {
        },
        initialize: function (args) {
        },
        isCreateFromFile : function() {return false;},
        isChangeMainFile : function() {return false;},
        isCreateFromTemplate : function() {return false;},
        isUpdate : function() {return false;},
        isSetAttachments : function() {return false;},
        isReady : function() {return false;},
        isSendReminder : function() {return false;},
        isCancel : function() {return false;},
        isRestart : function() {return false;},
        isProlong : function() {return false;},
        isDelete : function() {return false;},
        isGet : function() {return false;},
        isGetHistory : function() {return false;},
        isList : function() {return false;},
        isDownloadFile : function() {return false;},
        isDownloadMainFile : function() {return false;},
        isAddToPadQueue : function() {return false;},
        isGetProfile : function() {return false;},
        isGetPaymentInfo : function() {return false;},
        isSetPassword : function() {return false;},
        isSetLanguage : function() {return false;},
        isSignup : function() {return false;},
        isSendPasswordResetMail : function() {return false;},
        isUpdateProfile : function() {return false;},
        isCheckClient : function() {return false;},
        isReject : function() {return false;},
        isSign : function() {return false;},
        name : function() {return this.get("name");},
        oauth : function() {return this.get("oauth");},
        authorization: function() { return this.oauth().authorizationForRequests();  },
        result : function() {return this.get("result");},
        setResult : function(result) {return this.set({"result" : result});},
        _call : function(urlsuffix, arg, authenticate) {
          arg.headers = { 'Client-Name' : "api-demo",
                          'Client-Time' : new Date().toISOString()
                        };
          if (authenticate) {
            arg.headers.authorization = this.authorization();
          }
          $.ajax(Scrive.apiUrl()+urlsuffix, arg);
        },
        unauthenticatedCall : function(urlsuffix, arg) {
          this._call(urlsuffix, arg, false);
        },
        call : function(urlsuffix, arg) {
          this._call(urlsuffix, arg, true);
        }
});



window.ApiCallView = function(args) {
        if (args.model.isCreateFromFile())
           return new CreateFromFileApiCallView(args);
        if (args.model.isChangeMainFile())
           return new ChangeMainFileApiCallView(args);
        else if (args.model.isCreateFromTemplate())
           return new CreateFromTemplateApiCallView(args);
        else if (args.model.isUpdate())
           return new UpdateApiCallView(args);
        else if (args.model.isSetAttachments())
           return new SetAttachmentsApiCallView(args);
        else if (args.model.isReady())
           return new ReadyApiCallView(args);
        else if (args.model.isSendReminder())
           return new SendReminderApiCallView(args);
        else if (args.model.isCancel())
           return new CancelApiCallView(args);
        else if (args.model.isRestart())
           return new RestartApiCallView(args);
        else if (args.model.isProlong())
           return new ProlongApiCallView(args);
        else if (args.model.isDelete())
           return new DeleteApiCallView(args);
        else if (args.model.isGet())
           return new GetApiCallView(args);
        else if (args.model.isGetHistory())
           return new GetHistoryApiCallView(args);
        else if (args.model.isList())
           return new ListApiCallView(args);
        else if (args.model.isDownloadFile())
           return new DownloadFileApiCallView(args);
        else if (args.model.isDownloadMainFile())
           return new DownloadMainFileApiCallView(args);
        else if (args.model.isAddToPadQueue())
           return new AddToPadQueueApiCallView(args);
        else if (args.model.isReject())
           return new RejectApiCallView(args);
        else if (args.model.isSign())
           return new SignApiCallView(args);
        else if (args.model.isGetProfile())
           return new GetProfileApiCallView(args);
        else if (args.model.isGetPaymentInfo())
           return new GetPaymentInfoApiCallView(args);
        else if (args.model.isSetLanguage())
           return new SetLanguageApiCallView(args);
        else if (args.model.isSetPassword())
           return new SetPasswordApiCallView(args);
        else if (args.model.isUpdateProfile())
           return new UpdateProfileApiCallView(args);
        else if (args.model.isSignup())
           return new SignupApiCallView(args);
        else if (args.model.isSendPasswordResetMail())
           return new SendPasswordResetMailApiCallView(args);
        else if (args.model.isCheckClient())
           return new CheckClientApiCallView(args);

}

})(window);
