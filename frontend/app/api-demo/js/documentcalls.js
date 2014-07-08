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
            this.call("createfromfile", {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
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
            this.call("changemainfile/" + this.documentid(), {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
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
            this.call("createfromtemplate/" + model.templateid(), {
                type: 'POST',
                cache: false,
                contentType: false,
                processData: false,
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

            this.call("update/" + model.documentid(), {
                type: 'POST',
                data:{json:model.json(), objectversion : this.objectversion()},
                cache: false,
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


window.SetAttachmentsApiCall = ApiCall.extend({
        defaults: {
            name : "Set author attachments for given draft",
            documentid : LocalStorage.get("api","documentid")
        },
        initialize: function (args) {
        },
        isSetAttachments : function() {return true;},
        multiFile : function() {return this.get("multiFile");},
        setMultiFile : function(multiFile) {
            return this.set({"multiFile" : multiFile});
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
             LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            $("body").append(form);

            var slaves = _.filter(this.multiFile()["slaves"],function(s) {return s != undefined;});
            var slavesParents = _.map(slaves,function(s) {return $(s).parent();});
            for(var i=0;i<slaves.length - 1;i++)
              form.append($(slaves[i]).attr('name',"attachment_"+ i));
            var formData = new FormData(form[0]);
            this.call("setattachments/" + this.documentid(), {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                    for(var i=0;i<slaves.length && i< slavesParents.length;i++)
                      slavesParents[i].append(slaves[i]);
                    form.remove();
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                    for(var i=0;i<slaves.length && i< slavesParents.length;i++)
                      slavesParents[i].append(slaves[i]);
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
            this.call("ready/" + model.documentid(), {
                type: 'POST',
                data : {json : '{ "timezone": "Europe/Stockholm" }' , objectversion : model.objectversion()},
                cache: false,
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
            this.call("remind/" + model.documentid(), {
                type: 'POST',
                cache: false,
                contentType: false,
                processData: false,
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.ForwardApiCall = ApiCall.extend({
        defaults: {
            name : "Forward email with signed document",
            documentid : LocalStorage.get("api","documentid"),
            email : "vip@gmail.com",
            noContent : undefined
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        email : function() {return this.get("email");},
        setEmail: function(email) {
            return this.set({"email" : email});
        },
        noContent : function() {return this.get("noContent");},
        setNoContent: function(bool) {
            return this.set({"noContent" : bool});
        },
        initialize: function (args) {
        },
        isForward : function() {return true;},
        send : function() {
            var model = this;
            this.call("forward/" + model.documentid(), {
                type: 'POST',
                cache: false,
                data : { email : model.email(), nocontent : this.noContent() ? "true" : undefined},
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
            this.call("cancel/" + model.documentid(), {
                type: 'POST',
                cache: false,
                data : {objectversion : model.objectversion()},
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.RestartApiCall = ApiCall.extend({
        defaults: {
            name : "Restart API call",
            documentid : LocalStorage.get("api","documentid"),
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        initialize: function (args) {
        },
        isRestart : function() {return true;},
        send : function() {
            var model = this;
            this.call("restart/" + model.documentid(), {
                type: 'POST',
                cache: false,
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.ProlongApiCall = ApiCall.extend({
        defaults: {
            name : "Prolong API call",
            documentid : LocalStorage.get("api","documentid"),
            days : 1
        },
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
            LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        days : function() {return this.get("days");},
        setDays : function(days) {
            return this.set({"days" : days});
        },
        initialize: function (args) {
        },
        isProlong : function() {return true;},
        send : function() {
            var model = this;
            this.call("prolong/" + model.documentid(), {
                type: 'POST',
                cache: false,
                data : {days : model.days()},
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
            this.call("delete/" + model.documentid(), {
                type: 'DELETE',
                cache: false,
                contentType: false,
                processData: false,
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
            this.call("get/" + model.documentid(), {
                type: 'GET',
                cache: false,
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.GetHistoryApiCall = ApiCall.extend({
        defaults: {
             name : "Get history API call",
             documentid : LocalStorage.get("api","documentid")

        },
        initialize: function (args) {
        },
        isGetHistory : function() {return true;},
        documentid : function() {return this.get("documentid");},
        setDocumentid : function(documentid) {
             LocalStorage.set("api","documentid",documentid);
            return this.set({"documentid" : documentid});
        },
        send : function() {
            var model = this;
            this.call("history/" + model.documentid(), {
                type: 'GET',
                cache: false,
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
            this.call("list", {
                type: 'GET',
                cache: false,
                data : { tags   : model.tags(),
                         offset : model.offset(),
                         limit  : model.limit(),
                         selectfilter : model.selectfilters(),
                         documentType : model.documentType()
                },
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
            this.call("downloadmainfile/" + model.documentid() + "/filename.pdf", {
                type: 'GET',
                cache: false,
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
            this.call("downloadfile/" + model.documentid() + "/" + model.fileid() + "/filename.pdf", {
                type: 'GET',
                cache: false,
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.ExtractTextsApiCall = ApiCall.extend({
        defaults: {
             name : "Extract texts API call",
             documentid : LocalStorage.get("api","documentid"),
             fileid : LocalStorage.get("api","fileid"),
             json : LocalStorage.get("api","extract-texts-json") || "{\"rects\": [{\"rect\":[0,0,1,1],\"page\": 1}]}"
        },
        initialize: function (args) {
        },
        isExtractTexts : function() {return true;},
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
        json : function() {return this.get("json");},
        setJson : function(json) {
             LocalStorage.set("api","extract-texts-json",json);
            return this.set({"json" : json});
        },
        send : function() {
            var model = this;
            this.call("extracttexts/" + model.documentid() + "/" + model.fileid(), {
                type: 'GET',
                data: {json: model.json()},
                cache: false,
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
            this.call("reject/" + model.documentid() + "/" + model.signatorylinkid(), {
                type: 'POST',
                cache: false,
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

window.SetSignatoryAttachmentApiCall = ApiCall.extend({
        defaults: {
            name : "Set a file for give signatory attachment",
            documentid : LocalStorage.get("api","documentid"),
            signatorylinkid : LocalStorage.get("api","signatorylinkid"),
            attachmentName : "Attachment name",
        },
        initialize: function (args) {
        },
        isSetSignatoryAttachment : function() {return true;},
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
        attachmentName : function() {return this.get("attachmentName");},
        setAttachmentName : function(attachmentName) {
            return this.set({"attachmentName" : attachmentName});
        },
        file : function() {return this.get("file");},
        setFile : function(file) {
            return this.set({"file" : file});
        },
        send : function() {
            var model = this;
            var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
            $("body").append(form);
            var slaves = _.filter(this.file()["slaves"],function(s) {return s != undefined;});
            for(var i=0;i<slaves.length - 1;i++)
              form.append($(slaves[i]).clone().attr('name',"file"));
            var formData = new FormData(form[0]);
            this.call("setsignatoryattachment/"+model.documentid() + "/" + model.signatorylinkid() + "/" + model.attachmentName(), {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
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
            this.call("sign/" + model.documentid() + "/" + model.signatorylinkid(), {
                type: 'POST',
                cache: false,
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

window.SetAttachmentsApiCallView = Backbone.View.extend({
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

            filebox = $("<div>");
            if (this.list == undefined || this.fileinput == undefined || this.fileinput.data("MultiFile") == undefined) {
              this.list = $("<div />");
              this.fileinput = $("<input class='multiFileInput' type='file'/>");
              this.fileinput.MultiFile({
                    list: this.list,
              });
              model.setMultiFile(this.fileinput.data("MultiFile"));

            }
            boxLeft.append($("<div>Document # : <BR/></div>").append(documentidInput)).append($("<div> File: <BR/> </div>").append(filebox.append(this.list).append(this.fileinput))).append($("<div/>").append(button));
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


window.ForwardApiCallView = Backbone.View.extend({
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
            var emailInput = $("<input type='text'/>").val(model.email());
            emailInput.change(function() {model.setEmail(emailInput.val()); return false;})

            var noContentCheckbox = $("<input type='checkbox' />");
            if (model.noContent())
              noContentCheckbox.prop('checked',model.noContent);
            noContentCheckbox.change(function() {model.setNoContent(noContentCheckbox.prop('checked'))});
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>Email : <BR/></div>").append(emailInput))
                   .append($("<div>No content : </div>").append(noContentCheckbox))
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


window.RestartApiCallView = Backbone.View.extend({
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
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
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

window.ProlongApiCallView = Backbone.View.extend({
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
            var daysInput = $("<input type='text'/>").val(model.days());
            daysInput.change(function() {model.setDays(daysInput.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
            boxLeft.append($("<div>Days : <BR/></div>").append(daysInput))
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


window.GetHistoryApiCallView = Backbone.View.extend({
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


window.ExtractTextsApiCallView = Backbone.View.extend({
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
            var jsontextarea = $("<textarea class='json-text-area'>"+model.json()+"</textarea>");
            jsontextarea.change(function() {model.setJson(jsontextarea.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>File #: <BR/></div>").append(fileidInput))
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
            var customTextImput = $("<textarea style='height:0px;border:0px;padding:0px;margin:0px;width:300px;'/>").text(model.customtext());

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


window.SetSignatoryAttachmentApiCallView = Backbone.View.extend({
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

            var signatorylinkidInput = $("<input type='text'/>").val(model.signatorylinkid());
            signatorylinkidInput.change(function() {model.setSignatorylinkid(signatorylinkidInput.val()); return false;})

            var attachmentNameInput = $("<input type='text'/>").val(model.attachmentName());
            attachmentNameInput.change(function() {model.setAttachmentName(attachmentNameInput.val()); return false;})


            this.filebox = $("<div>");
            if (this.list == undefined || this.fileinput == undefined || this.fileinput.data("MultiFile") == undefined) {
              this.list = $("<div />");
              this.fileinput = $("<input class='multiFileInput' type='file'/>");
              this.fileinput.MultiFile({
                    list: this.list,
                    limit : 1
              });
              model.setFile(this.fileinput.data("MultiFile"));

            }


            this.filebox = $("<div>");

            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput))
                   .append($("<div>Signatory #: <BR/></div>").append(signatorylinkidInput))
                   .append($("<div>Attachment name (need to be exact) : <BR/></div>").append(attachmentNameInput))
                   .append($("<div>File (don't upload to delete)) : <BR/></div>").append(this.filebox.append(this.list).append(this.fileinput)))
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
