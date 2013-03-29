/*
 * API demo main model + view
 */


(function(window) {

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

window.ChangeMainFileApiCall = ApiCall.extend({
        defaults: {
            name : "Change main file API call",
            file : $("<input type='file' name='file' class='file-input'/>"),
            documentid : LocalStorage.get("api","documentid")
        },
        initialize: function (args) {
        },
        isChangeMainFile : function() {return true;},
        file : function() {return this.get("file");},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
             LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            $("body").append(form);
            form.append(this.file());
            var formData = new FormData(form[0]);
            $.ajax(Scrive.apiUrl()+"changemainfile/" + this.documentid(), {
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
            objectversion : "",
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
        objectversion : function() {return this.get("objectversion");},
        setObjectversion : function(objectversion) {
            return this.set({"objectversion" : objectversion});
        },

        json : function() {return this.get("json");},
        setJson : function(json) {return this.set({"json" : json});},
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            form.attr('action', Scrive.apiUrl()+"update/" + model.documentid());

            $("body").append(form);
            form.append($("<input type='hidden' name='json'/>").val(model.json()));

            //var formData = new FormData(form[0]);
            $.ajax(Scrive.apiUrl()+"update/" + model.documentid(), {
                type: 'POST',
                data:{json:model.json(), objectversion : this.objectversion()},
                //data: formData,
                cache: false,
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
        objectversion : function() {return this.get("objectversion");},
        setObjectversion : function(objectversion) {
            return this.set({"objectversion" : objectversion});
        },
        initialize: function (args) {
        },
        isReady : function() {return true;},
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            form.attr('action', Scrive.apiUrl()+"ready/" + model.documentid());
            $("body").append(form);
            form.append($("<input type='hidden' name='json'/>").val('{ "timezone": "Europe/Stockholm" }'));
            $.ajax(Scrive.apiUrl()+"ready/" + model.documentid(), {
                type: 'POST',
                data : {json : '{ "timezone": "Europe/Stockholm" }' , objectversion : model.objectversion()},
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
            documentid : LocalStorage.get("api","documentid"),
            objectversion : ""
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        objectversion : function() {return this.get("objectversion");},
        setObjectversion : function(objectversion) {
            return this.set({"objectversion" : objectversion});
        },

        initialize: function (args) {
        },
        isCancel : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"cancel/" + model.documentid(), {
                type: 'POST',
                cache: false,
                data : {objectversion : model.objectversion()},
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
             tags : "[]",
             offset : 0,
             limit : 5,
             selectfilters : "[{\"name\":\"cansign\",\"value\":\"\"}]",
             documentType : ""
        },
        tags : function() {return this.get("tags");},
        setTags : function(tags) {
            LocalStorage.set("api","tags",tags);
            this.set({"tags" : tags});
        },
        offset : function() {return this.get("offset");},
        setOffset : function(offset) {
            LocalStorage.set("api","offset",offset);
            this.set({"offset" : offset});
        },
        limit : function() {return this.get("limit");},
        setLimit : function(limit) {
            LocalStorage.set("api","limit",limit);
            this.set({"limit" : limit});
        },
        selectfilters : function() {return this.get("selectfilters");},
        setSelectfilters : function(selectfilters) {
            LocalStorage.set("api","selectfilters",selectfilters);
            this.set({"selectfilters" : selectfilters});
        },
        documentType : function() {return this.get("documentType");},
        setDocumentType : function(documentType) {
            LocalStorage.set("api","documentType",documentType);
            this.set({"documentType" : documentType});
        },
        initialize: function (args) {
        },
        isList : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"list", {
                type: 'GET',
                cache: false,
                data : { tags   : model.tags(),
                         offset : model.offset(),
                         limit  : model.limit(),
                         selectfilter : model.selectfilters(),
                         documentType : model.documentType()
                },
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

window.DownloadFileApiCall = ApiCall.extend({
        defaults: {
             name : "Download file API call",
             documentid : LocalStorage.get("api","documentid"),
             fileid : LocalStorage.get("api","fileid")
        },
        initialize: function (args) {
        },
        isDownloadFile : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
             LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        fileid : function() {return this.get("fileid");},
        setFileid : function(fileid) {
             LocalStorage.set("api","fileid",fileid);
            return this.set({"fileid" : fileid});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"downloadfile/" + model.documentid() + "/" + model.fileid() + "/filename.pdf", {
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
             documentid : LocalStorage.get("api","documentid"),
             signatorylinkid : LocalStorage.get("api","signatorylinkid")

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
        setSignatorylinkid : function(signatorylinkid) {
            LocalStorage.set("api","signatorylinkid",signatorylinkid);
            return this.set({"signatorylinkid" : signatorylinkid});
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

window.RejectApiCall = ApiCall.extend({
        defaults: {
             name : "Reject document by signatory",
             documentid : LocalStorage.get("api","documentid"),
             signatorylinkid : LocalStorage.get("api","signatorylinkid"),
             customtext : ""

        },
        initialize: function (args) {
        },
        isReject : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        signatorylinkid : function() {return this.get("signatorylinkid");},
        setSignatorylinkid : function(signatorylinkid) {
            LocalStorage.set("api","signatorylinkid",signatorylinkid);
            return this.set({"signatorylinkid" : signatorylinkid});
        },
        customtext : function() {return this.get("customtext");},
        setCustomtext : function(customtext) {
             return this.set({"customtext" : customtext});
        },
        objectversion : function() {return this.get("objectversion");},
        setObjectversion : function(objectversion) {
            return this.set({"objectversion" : objectversion});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"reject/" + model.documentid() + "/" + model.signatorylinkid(), {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                data : {
                     customtext : this.customtext(),
                     objectversion : model.objectversion()
                    },
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.SignApiCall = ApiCall.extend({
        defaults: {
             name : "Sign document by signatory",
             documentid : LocalStorage.get("api","documentid"),
             signatorylinkid : LocalStorage.get("api","signatorylinkid"),
             screenshotFirst : "",
             screenshotSigning : "",
             fields : "[]"
        },
        initialize: function (args) {
        },
        isSign : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        signatorylinkid : function() {return this.get("signatorylinkid");},
        setSignatorylinkid : function(signatorylinkid) {
            LocalStorage.set("api","signatorylinkid",signatorylinkid);
            return this.set({"signatorylinkid" : signatorylinkid});
        },
        screenshotFirst : function() {return this.get("screenshotFirst");},
        setScreenshotFirst : function(screenshotFirst) {
            LocalStorage.set("api","screenshotFirst",screenshotFirst);
            return this.set({"screenshotFirst" : screenshotFirst});
        },
        screenshotSigning : function() {return this.get("screenshotSigning");},
        setScreenshotSigning : function(screenshotSigning) {
            LocalStorage.set("api","screenshotSigning",screenshotSigning);
            return this.set({"screenshotSigning" : screenshotSigning});
        },

        fields : function() {return this.get("fields");},
        setFields : function(fields) {
            LocalStorage.set("api","fields",fields);
            return this.set({"fields" : fields});
        },
        objectversion : function() {return this.get("objectversion");},
        setObjectversion : function(objectversion) {
            return this.set({"objectversion" : objectversion});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"sign/" + model.documentid() + "/" + model.signatorylinkid(), {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                data : {
                     screenshots : JSON.stringify({first : [new Date().toISOString(),this.screenshotFirst()] , signing : [new Date().toISOString(),this.screenshotSigning()] }),
                     fields : this.fields(),
                     objectversion : model.objectversion()
                    },
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});





window.CreateFromFileApiCallView = Backbone.View.extend({
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

window.ChangeMainFileApiCallView = Backbone.View.extend({
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
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})

            this.filebox = $("<div>");
            boxLeft.append($("<div>Document # : <BR/></div>").append(documentidInput)).append($("<div> File: <BR/> </div>").append(this.filebox.append(model.file()))).append($("<div/>").append(button));
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

window.CreateFromTemplateApiCallView = Backbone.View.extend({
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

window.UpdateApiCallView = Backbone.View.extend({
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

            var objectversionInput = $("<input type='text'/>").val(model.objectversion());
            objectversionInput.change(function() {model.setObjectversion(objectversionInput.val()); return false;})

            var jsontextarea = $("<textarea class='json-text-area'>"+model.json()+"</textarea>");
            jsontextarea.change(function() {model.setJson(jsontextarea.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document # : <BR/></div>").append(documentidInput))
                   .append($("<div>Object version (optional) : <BR/></div>").append(objectversionInput))
                   .append($("<div>JSON : <BR/></div>").append(jsontextarea))
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

window.ReadyApiCallView = Backbone.View.extend({
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

            var objectversionInput = $("<input type='text'/>").val(model.objectversion());
            objectversionInput.change(function() {model.setObjectversion(objectversionInput.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                               .append($("<div>Object version (optional): <BR/></div>").append(objectversionInput))
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

window.SendReminderApiCallView = Backbone.View.extend({
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



window.CancelApiCallView = Backbone.View.extend({
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

            var objectversionInput = $("<input type='text'/>").val(model.objectversion());
            objectversionInput.change(function() {model.setObjectversion(objectversionInput.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                                           .append($("<div>Object version (optional) : <BR/></div>").append(objectversionInput))
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


window.DeleteApiCallView = Backbone.View.extend({
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

window.GetApiCallView = Backbone.View.extend({
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

window.ListApiCallView = Backbone.View.extend({
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
            var offsetInput = $("<input type='text'/>").val(model.offset());
            offsetInput.change(function() {model.setOffset(offsetInput.val()); return false;})
            var limitInput = $("<input type='text'/>").val(model.limit());
            limitInput.change(function() {model.setLimit(limitInput.val()); return false;})
            var selectfiltersInput = $("<input type='text'/>").val(model.selectfilters());
            selectfiltersInput.change(function() {model.setSelectfilters(selectfiltersInput.val()); return false;})
            var documentTypeInput = $("<input type='text'/>").val(model.documentType());
            documentTypeInput.change(function() {model.setDocumentType(documentTypeInput.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Tags: <BR/></div>").append(tagsInput));
            boxLeft.append($("<div>Offset: <BR/></div>").append(offsetInput));
            boxLeft.append($("<div>Limit: <BR/></div>").append(limitInput));
            boxLeft.append($("<div>Select filters: <BR/></div>").append(selectfiltersInput));
            boxLeft.append($("<div>Document type: <BR/></div>").append(documentTypeInput));
            boxLeft.append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

window.DownloadMainFileApiCallView = Backbone.View.extend({
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

window.DownloadFileApiCallView = Backbone.View.extend({
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
            var fileidInput = $("<input type='text'/>").val(model.fileid());
            fileidInput.change(function() {model.setFileid(fileidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>File #: <BR/></div>").append(fileidInput))
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


window.AddToPadQueueApiCallView = Backbone.View.extend({
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

window.RejectApiCallView = Backbone.View.extend({
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
            var objectversionInput = $("<input type='text'/>").val(model.objectversion());
            objectversionInput.change(function() {model.setObjectversion(objectversionInput.val()); return false;})
            var customTextImput = $("<textarea style='height:0px;border:0px;padding:0px;margin:0px;width:300px;'/>").html(model.customtext());
            customTextImput.tinymce({
                      script_url: '/tiny_mce/tiny_mce.js',
                      theme: "advanced",
                      theme_advanced_toolbar_location: "top",
                      theme_advanced_buttons1: "bold,italic,underline,separator,strikethrough,bullist,numlist,separator,undo,redo,separator,cut,copy,paste",
                      theme_advanced_buttons2: "",
                      convert_urls: false,
                      theme_advanced_toolbar_align: "middle",
                      plugins: "noneditable,paste",
                      valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul"
            });


            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.setCustomtext(customTextImput.val()); model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>Signatory #: <BR/></div>").append(signatorylinkidInput))
                   .append($("<div>Object version (optional) : <BR/></div>").append(objectversionInput))
                   .append($("<div>Custom text (empty == Ignore): <BR/></div>").append(customTextImput))
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

window.SignApiCallView = Backbone.View.extend({
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

            var objectversionInput = $("<input type='text'/>").val(model.objectversion());
            objectversionInput.change(function() {model.setObjectversion(objectversionInput.val()); return false;})

            var screenshotFirstInput = $("<input type='text'/>").val(model.screenshotFirst());
            screenshotFirstInput.change(function() {model.setScreenshotFirst(screenshotFirstInput.val()); return false;})

            var screenshotSigningInput = $("<input type='text'/>").val(model.screenshotSigning());
            screenshotSigningInput.change(function() {model.setScreenshotSigning(screenshotSigningInput.val()); return false;})

            var fieldsInput = $("<textarea class='json-text-area'>"+model.fields()+"</textarea>")
            fieldsInput.change(function() {model.setFields(fieldsInput.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>Signatory #: <BR/></div>").append(signatorylinkidInput))
                   .append($("<div>Object version (optional) : <BR/></div>").append(objectversionInput))
                   .append($("<div>First screenshot (optional) : <BR/></div>").append(screenshotFirstInput))
                   .append($("<div>Signing screenshot (optional) : <BR/></div>").append(screenshotSigningInput))
                   .append($("<div>Fields #: <BR/></div>").append(fieldsInput))
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
