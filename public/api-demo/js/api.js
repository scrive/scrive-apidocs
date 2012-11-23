/*
 * API demo main model + view
 */


(function(window) {

var ApiCall = Backbone.Model.extend({
        defaults: {
        },
        initialize: function (args) {
        },
        isCreateFromFile : function() {return false;},
        isCreateFromTemplate : function() {return false;},
        isUpdate : function() {return false;},
        isReady : function() {return false;},
        isSendReminder : function() {return false;},                           
        isCancel : function() {return false;},
        isDelete : function() {return false;},
        isGet : function() {return false;},
        isList : function() {return false;},
        isDownloadMainFile : function() {return false;},
        isAddToPadQueue : function() {return false;},
        name : function() {return this.get("name");},
        oauth : function() {return this.get("oauth");},
        authorization: function() { return this.oauth().authorizationForRequests();  },
        result : function() {return this.get("result");},
        setResult : function(result) {return this.set({"result" : result});}
});
window.CreateFromFileApiCall = ApiCall.extend({
        defaults: {
            name : "Create from file API call",
            file : $("<input type='file' name='file' class='file-input'/>"),
        },
        initialize: function (args) {
        },
        isCreateFromFile : function() {return true;},
        file : function() {return this.get("file");},
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            $("body").append(form);
            form.append(this.file());
            var formData = new FormData(form[0]);
            $.ajax(Scrive.apiUrl()+"createfromfile", {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.file().detach();
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                    form.remove();
                },
                error : function(res) {
                    model.file().detach();
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                    form.remove();
                }
            });

        }                                     
});

window.CreateFromTemplateApiCall = ApiCall.extend({
        defaults: {
            name : "Create from template API call",
            templateid : "0000"
        },
        initialize: function (args) {
        },
        isCreateFromTemplate : function() {return true;},
        templateid : function() {return this.get("templateid");},
        setTemplateid : function(templateid) {return this.set({"templateid" : templateid});},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"createfromtemplate/" + model.templateid(), {
                type: 'POST',
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });

        }                     
});

window.UpdateApiCall = ApiCall.extend({
        defaults: {
            name : "Update API call",
            json : "{}",
            documentid : LocalStorage.get("api","documentid")
        },
        initialize: function (args) {
        },
        isUpdate : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        json : function() {return this.get("json");},
        setJson : function(json) {return this.set({"json" : json});},
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            $("body").append(form);
            form.append($("<input type='hidden' name='json'/>").val(model.json()));
            var formData = new FormData(form[0]);
            $.ajax(Scrive.apiUrl()+"update/" + model.documentid(), {
                type: 'POST',
                data: formData,
                cache: false,
                processData: false,
                contentType: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                    form.remove();
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                    form.remove();
                }
            });
        }   
});

window.ReadyApiCall = ApiCall.extend({
        defaults: {
            name : "Ready API call",
            documentid : LocalStorage.get("api","documentid")
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
      
        initialize: function (args) {
        },
        isReady : function() {return true;},
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            $("body").append(form);
            form.append($("<input type='hidden' name='json'/>").val('{ "timezone": "Europe/Stockholm" }'));
            var formData = new FormData(form[0]);
            $.ajax(Scrive.apiUrl()+"ready/" + model.documentid(), {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }    
});

window.SendReminderApiCall = ApiCall.extend({
        defaults: {
            name : "Send reminder API call",
            documentid : LocalStorage.get("api","documentid")
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },

        initialize: function (args) {
        },
        isSendReminder : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"remind/" + model.documentid(), {
                type: 'POST',
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.CancelApiCall = ApiCall.extend({
        defaults: {
            name : "Cancel API call",
            documentid : LocalStorage.get("api","documentid")
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },

        initialize: function (args) {
        },
        isCancel : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"cancel/" + model.documentid(), {
                type: 'POST',
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.DeleteApiCall = ApiCall.extend({
        defaults: {
            name : "Delete API call",
            documentid : LocalStorage.get("api","documentid")
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },

        initialize: function (args) {
        },
        isDelete : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"delete/" + model.documentid(), {
                type: 'DELETE',
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});


window.GetApiCall = ApiCall.extend({
        defaults: {
             name : "Get API call",
             documentid : LocalStorage.get("api","documentid")

        },
        initialize: function (args) {
        },
        isGet : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
             LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"get/" + model.documentid(), {
                type: 'GET',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }    
});

window.ListApiCall = ApiCall.extend({
        defaults: {
             name : "List API call",
             tags : "[]"
        },
        tags : function() {return this.get("tags");},
        setTags : function(tags) {
            LocalStorage.set("api","tags",tags);
            this.set({"tags" : tags});
        },
        initialize: function (args) {
        },
        isList : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"list", {
                type: 'GET',
                cache: false,
                data : { tags : model.tags() },
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});


window.DownloadMainFileApiCall = ApiCall.extend({
        defaults: {
             name : "Download main file API call",
             documentid : LocalStorage.get("api","documentid")

        },
        initialize: function (args) {
        },
        isDownloadMainFile : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
             LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"downloadmainfile/" + model.documentid() + "/filename.pdf", {
                type: 'GET',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.AddToPadQueueApiCall = ApiCall.extend({
        defaults: {
             name : "Add to padqueue API call",
             documentid : LocalStorage.get("api","documentid")

        },
        initialize: function (args) {
        },
        isAddToPadQueue : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        signatorylinkid : function() {return this.get("signatorylinkid");},
        setSignatorylinkid : function(documentid) {
            LocalStorage.set("api","signatorylinkid",documentid);
            return this.set({"signatorylinkid" : documentid});
        },
        
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"padqueue/add/" + model.documentid() + "/" + model.signatorylinkid(), {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});


window.ApiCallView = function(args) {
        if (args.model.isCreateFromFile())
           return new CreateFromFileApiCallView(args);
        else if (args.model.isCreateFromTemplate())
           return new CreateFromTemplateApiCallView(args);
        else if (args.model.isUpdate())
           return new UpdateApiCallView(args);
        else if (args.model.isReady())
           return new ReadyApiCallView(args);
        else if (args.model.isSendReminder())
           return new SendReminderApiCallView(args);
        else if (args.model.isCancel())
           return new CancelApiCallView(args);
        else if (args.model.isDelete())
           return new DeleteApiCallView(args);
        else if (args.model.isGet())
           return new GetApiCallView(args);
        else if (args.model.isList())
           return new ListApiCallView(args);
        else if (args.model.isDownloadMainFile())
           return new DownloadMainFileApiCallView(args);
        else if (args.model.isAddToPadQueue())
           return new AddToPadQueueApiCallView(args);
}


var CreateFromFileApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            this.filebox = $("<div>");
            boxLeft.append($("<div> File: <BR/> </div>").append(this.filebox.append(model.file()))).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
            this.filebox.append(model.file())
                
        }
});

var CreateFromTemplateApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var templateidInput = $("<input type='text'/>").val(model.templateid());
            templateidInput.change(function() {model.setTemplateid(templateidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Template #: <BR/></div>").append(templateidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var UpdateApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var jsontextarea = $("<textarea class='json-text-area'>"+model.json()+"</textarea>");
            jsontextarea.change(function() {model.setJson(jsontextarea.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document # : <BR/></div>").append(documentidInput)).append($("<div>JSON : <BR/></div>").append(jsontextarea)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var ReadyApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var SendReminderApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});



var CancelApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});


var DeleteApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var GetApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
          
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var ListApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var tagsInput = $("<input type='text'/>").val(model.tags());
            tagsInput.change(function() {model.setTags(tagsInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Tags: <BR/></div>").append(tagsInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});    

var DownloadMainFileApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var AddToPadQueueApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var signatorylinkidInput = $("<input type='text'/>").val(model.signatorylinkid());
            signatorylinkidInput.change(function() {model.setSignatorylinkid(signatorylinkidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>Signatory #: <BR/></div>").append(signatorylinkidInput))
                   .append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});


})(window);
