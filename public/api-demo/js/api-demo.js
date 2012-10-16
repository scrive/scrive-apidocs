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
            select.append(none).append(createFromFile).append(createFromTemplate).append(update).append(ready).append(sendReminder).append(cancel).append(del).append(check).append(list);
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
