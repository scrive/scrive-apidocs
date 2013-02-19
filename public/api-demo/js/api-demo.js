/*
 * API demo main model + view
 */


(function(window) {

var ApiDemoModel = Backbone.Model.extend({
        defaults: {
        },
        initialize: function (args) {
            var model = this;
            var oauth = new OAuth();
            oauth.model().bind('ready', function() {model.trigger('change');});
            oauth.model().bind('clear', function() {model.trigger('change');})

            this.set({oauth : oauth})
        },
        oauth : function() {
            return this.get('oauth');
        },
        selectedApiCall : function() {
            return this.get("selectedApiCall");
        },
        setSelectedApiCall : function(sac) {
            this.set({selectedApiCall : sac});
        }
});


var ApiDemoView = Backbone.View.extend({
        model: ApiDemoModel,
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        apisection : function() {
            var model = this.model;
            var oauth = model.oauth();
            var select = $("<select></select>");
            var none = $("<option value=''></option>")
            var createFromFile = $("<option value='cff' >Create from file</option>");
            var createFromTemplate = $("<option value='cft'>Create from template</option>");
            var update = $("<option value='u'>Update</option>");
            var ready = $("<option value='g'>Ready</option>");
            var sendReminder = $("<option value='s'>Send reminder</option>");
            var cancel = $("<option value='cc'>Cancel</option>");
            var del = $("<option value='d'>Delete</option>");
            var check = $("<option value='c'>Get</option>");
            var list  = $("<option value='l'>List</option>");
            var download  = $("<option value='dl'>Download file</option>");
            var addtopad  = $("<option value='pq'>Add to padqueue</option>");
            var profile  = $("<option value='gp'>Get profile</option>");
            var language  = $("<option value='sl'>Set language</option>");
            var password  = $("<option value='sp'>Set password</option>");
            var updateprofile = $("<option value='up'>Update profile</option>");
            var signup = $("<option value='su'>Signup</option>");
            var checkclient  = $("<option value='check'>Check client</option>");

            select.append(none).append(createFromFile).append(createFromTemplate).append(update)
                  .append(ready).append(sendReminder).append(cancel).append(del).append(check)
                  .append(list).append(download).append(addtopad)
                  .append($("<option>---------</option>"))
                  .append(profile).append(language).append(password).append(signup).append(updateprofile)
                  .append($("<option>---------</option>"))
                  .append(checkclient);
            if (model.selectedApiCall() != undefined) {
                if (model.selectedApiCall().isCreateFromFile())
                    createFromFile.attr("selected", "true");
                else if (model.selectedApiCall().isCreateFromTemplate())
                    createFromTemplate.attr("selected", "true");
                else if (model.selectedApiCall().isUpdate())
                    update.attr("selected", "true");
                else if (model.selectedApiCall().isReady())
                    ready.attr("selected", "true");
                else if (model.selectedApiCall().isSendReminder())
                    sendReminder.attr("selected", "true");
                else if (model.selectedApiCall().isCancel())
                    cancel.attr("selected", "true");
                else if (model.selectedApiCall().isDelete())
                    del.attr("selected", "true");
                else if (model.selectedApiCall().isGet())
                    check.attr("selected", "true");
                else if (model.selectedApiCall().isList())
                    list.attr("selected", "true");
                else if (model.selectedApiCall().isDownloadMainFile())
                    download.attr("selected", "true");
                else if (model.selectedApiCall().isAddToPadQueue())
                    addtopad.attr("selected", "true");
                else if (model.selectedApiCall().isGetProfile())
                    profile.attr("selected", "true");
                else if (model.selectedApiCall().isSetLanguage())
                    language.attr("selected", "true");
                else if (model.selectedApiCall().isSetPassword())
                    password.attr("selected", "true");
                else if (model.selectedApiCall().isSignup())
                    signup.attr("selected", "true");
                else if (model.selectedApiCall().isUpdateProfile())
                    updateprofile.attr("selected", "true");
                else if (model.selectedApiCall().isCheckClient())
                    checkclient.attr("selected", "true");
                else
                    none.attr("selected", "true");
            }
            select.change(function() {
                var v = select.val();
                if (select.val() == "cff")
                    model.setSelectedApiCall(new CreateFromFileApiCall({oauth : oauth}));
                else if (select.val() == "cft")
                    model.setSelectedApiCall(new CreateFromTemplateApiCall({oauth : oauth}));
                else if (select.val() == "u")
                    model.setSelectedApiCall(new UpdateApiCall({oauth : oauth}));
                else if (select.val() == "g")
                    model.setSelectedApiCall(new ReadyApiCall({oauth : oauth}));
                else if (select.val() == "s")
                    model.setSelectedApiCall(new SendReminderApiCall({oauth : oauth}));
                else if (select.val() == "cc")
                    model.setSelectedApiCall(new CancelApiCall({oauth : oauth}));
                else if (select.val() == "d")
                    model.setSelectedApiCall(new DeleteApiCall({oauth : oauth}));
                else if (select.val() == "c")
                    model.setSelectedApiCall(new GetApiCall({oauth : oauth}));
                else if (select.val() == "l")
                    model.setSelectedApiCall(new ListApiCall({oauth : oauth}));
                else if (select.val() == "dl")
                    model.setSelectedApiCall(new DownloadMainFileApiCall({oauth : oauth}));
                else if (select.val() == "pq")
                    model.setSelectedApiCall(new AddToPadQueueApiCall({oauth : oauth}));
                else if (select.val() == "gp")
                    model.setSelectedApiCall(new GetProfileApiCall({oauth : oauth}));
                else if (select.val() == "sl")
                    model.setSelectedApiCall(new SetLanguageApiCall({oauth : oauth}));
                else if (select.val() == "sp")
                    model.setSelectedApiCall(new SetPasswordApiCall({oauth : oauth}));
                else if (select.val() == "up")
                    model.setSelectedApiCall(new UpdateProfileApiCall({oauth : oauth}));
                else if (select.val() == "su")
                    model.setSelectedApiCall(new SignupApiCall({oauth : oauth}));
                else if (select.val() == "check")
                    model.setSelectedApiCall(new CheckClientApiCall({oauth : oauth}));

                else
                    model.setSelectedApiCall(undefined);
                return false;
            });
            var box = $("<div>");
            var selectbox = $("<div class='section'><div class='label'>Select API call: </div></div>").append(select);
            box.append(selectbox);
            if (model.selectedApiCall() != undefined) {
                var apicallbox = $("<div class='section'/>").append($("<div class='header'>").text(model.selectedApiCall().name()));
                apicallbox.append(new ApiCallView({model: model.selectedApiCall(), el: "<div/>"}).el);
                box.append(apicallbox);
            }
            return box;
        },
        render: function() {
            $(this.el).children().detach();
            var view = this;
            var model = this.model;
            var container = $(this.el);
            container.children().detach();
            container.append(model.oauth().view().el);
            var apiSection = $("<div class='api'>");
            apiSection.append("<div class='mainheader'>API</div>");
            if (model.oauth().ready())   {
              apiSection.append(this.apisection());
            } else {
                apiSection.append("<div>Please follow the steps in the box above in order to perform API calls.</div>");
            }
            container.append(apiSection);
            return this;

        }
});


window.ApiDemo = function() {
        var model = new ApiDemoModel({ });
        var view  = new ApiDemoView({
                            model: model,
                            el: $("<div class='main'/>")
                    });
        return {
                model : function() {return model;},
                view : function() {return view;}
        };
}

})(window);
