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
        isGet : function() {return false;},
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
            $.ajax(Scrive.serverUrl()+"/api/createfromfile", {
                type: 'POST',
                data: formData,
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.file().detach();
                    model.setResult(niceJSONText(JSON.parse(res),""));
                    form.remove();
                },
                error : function(res) {
                    model.file().detach();
                    model.setResult(niceJSONText(JSON.parse(res.responseText),""));form.remove();
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
            $.ajax(Scrive.serverUrl()+"/api/createfromtemplate/" + model.templateid(), {
                type: 'POST',
                data: {},
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(niceJSONText(JSON.parse(res),""));
                },
                error : function(res) {
                    model.setResult(niceJSONText(res.responseText,""));
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
            $.ajax(Scrive.serverUrl()+"/api/update/" + model.documentid(), {
                type: 'POST',
                data: formData,
                cache: false,
                processData: false,
                contentType: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(niceJSONText(JSON.parse(res),""));
                    form.remove();
                },
                error : function(res) {
                    model.setResult(niceJSONText(res.responseText,""));
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
            $.ajax(Scrive.serverUrl()+"/api/ready/" + model.documentid(), {
                type: 'POST',
                data: {},
                cache: false,
                contentType: false,
                processData: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(niceJSONText(JSON.parse(res),""));
                },
                error : function(res) {
                    model.setResult(niceJSONText(res.responseText,""));
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
            $.ajax(Scrive.serverUrl()+"/api/get/" + model.documentid(), {
                type: 'GET',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(niceJSONText(JSON.parse(res),""));
                },
                error : function(res) {
                    model.setResult(niceJSONText(res.responseText,""));
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
        else if (args.model.isGet())
           return new GetApiCallView(args);
}


var CreateFromFileApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        render : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            var boxRight = $("<div class='right-box'>");
            box.append(boxRight).append(boxLeft);
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div> File: <BR/> </div>").append(model.file())).append($("<div/>").append(button));
            if (model.result() != undefined)
                boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var CreateFromTemplateApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        render : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            var boxRight = $("<div class='right-box'>");
            box.append(boxRight).append(boxLeft);
            var templateidInput = $("<input type='text'/>").val(model.templateid());
            templateidInput.change(function() {model.setTemplateid(templateidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Template #: <BR/></div>").append(templateidInput)).append($("<div/>").append(button));
            if (model.result() != undefined)
                boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var UpdateApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        render : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            var boxRight = $("<div class='right-box'>");
            box.append(boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var jsontextarea = $("<textarea class='json-text-area'>"+model.json()+"</textarea>");
            jsontextarea.change(function() {model.setJson(jsontextarea.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document # : <BR/></div>").append(documentidInput)).append($("<div>JSON : <BR/></div>").append(jsontextarea)).append($("<div/>").append(button));
            if (model.result() != undefined)
                boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var ReadyApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        render : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            var boxRight = $("<div class='right-box'>");
            box.append(boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            if (model.result() != undefined)
                boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

var GetApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        render : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            var boxRight = $("<div class='right-box'>");
            box.append(boxRight).append(boxLeft);
            var documentidInput = $("<input type='text'/>").val(model.documentid());
            documentidInput.change(function() {model.setDocumentid(documentidInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Document #: <BR/></div>").append(documentidInput)).append($("<div/>").append(button));
            if (model.result() != undefined)
                boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});


window.niceJSONText = function(obj,s) {
    if (s == undefined) s = ""
    var res = "";
    if (obj instanceof Array) {
        if (obj.length == 0) return "[]";
        res += "[\n";
        for(var i=0;i<obj.length;i++) {
            res += (s + niceJSONText(obj[i], s + " ") + "\n")
            if (i<obj.length - 1)
                res += ",\n"
            else
                res += "\n"
        }    
        res += s + "]";
        
    } else if(typeof obj =='object' && obj != undefined) {
        var ks = _.keys(obj)
        if (ks.length == 0) return "{}";
        res += "{\n";
        for(var i=0;i<ks.length;i++) {
            res += (s + '"' + ks[i] + '"' + ":" + niceJSONText(obj[ks[i]], s + " "));
            if (i<ks.length - 1)
                res += ",\n"
            else
                res += "\n"
        }
        res += s + "}";

    } else if (typeof obj =='string') {
       res += '"' + obj + '"';
    } else {
        res += obj;
    }    
    return res
        
    
        
    

    
}
})(window);
