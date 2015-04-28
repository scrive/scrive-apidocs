
(function (window) {

window.AbstractAPICall = Backbone.Model.extend({
  defaults: {
    method: "GET",
    needsAuthorization: true,
    category: "other",
    tryToUseDocumentIDWithCopy: false,
    expectBinaryResponse: false
  },
  name: function () {
          return this.get("name");
        },
  description: function () {
          return this.get("description");
        },
  sampleUrl: function () {
          return this.get("sampleUrl");
        },
  method: function () {
          return this.get("method");
        },
  apiVersion: function () {
          return this.get("apiVersion");
        },
  needsAuthorization: function () {
          return this.get("needsAuthorization");
        },
  hasCategory: function (c) {
          if (typeof this.get("category") == "string" || this.get("category") instanceof String) {
            return c == this.get("category");
          } else if (this.get("category") instanceof Array) {
            return _.contains(this.get("category"), c);
          }
        },
  hasFileParams: function () {
          return _.any(this.params(), function (p) {
            return p.type() == "file";
          });
        },
  tryToUseDocumentIDWithCopy: function () {
          return this.get("tryToUseDocumentIDWithCopy");
        },
  expectBinaryResponse: function () {
          return this.get("expectBinaryResponse");
        },
  urlHash: function () {
          return this.name().replace(/\s+/g, "-").toLowerCase();
        },
  params: function () {
          return this.get("params");
        },
  createCall: function (props) {
          props = props || {};
          props.callPrototype = this;
          return new (this.get("constructor_"))(props);
        }
});

window.ApiCallInstance = AbstractAPICall.extend({
  defaults: {
    send: false,
    details: undefined,
    resultContent: undefined,
    responseStatusCode: undefined
  },
  initialize: function (args) {
    var self = this;
    _.each(args.params, function (p) {
      self.setParamValue(p, p.defaultValue());
    });
  },
  unsend: function () {
          this.set({
            send: false,
            details: undefined,
            resultContent: undefined,
            responseStatusCode: undefined
          });
          this.trigger("send");
        },
  isSent: function () {
          return this;
        },
  callPrototype: function () {
          return this.get("callPrototype");
        },
  getParamValue: function (p) {
          return this.get(p.argName())
        },
  setParamValue: function (p, v) {
          this.set(p.argName(), v);
          if (p.useLocalStorage()) {
            LocalStorage.set("param", p.argName(), v);
          }
        },
  getCallArgs: function () {
          var self = this;
          var args = {};
          _.each(this.params(), function (p) {
            if (p.sendAsParam()) {
              args[p.argName()] = self.getParamValue(p);
            }
          });
          return args;
        },
  attachFileParamsToForm: function (form, p) {
    var multifile = this.getParamValue(p);
    this.slaves = _.filter(multifile.slaves, function (s) {return s != undefined;});
    this.slavesParents = _.map(this.slaves, function (s) {return $(s).parent();});
    for (var i = 0;i < this.slaves.length - 1;i++) {
      form.append($(this.slaves[i]).attr("name", p.argName(i)));
    }
  },
  detachFileParamsFromForm: function (form) {
    for (var i = 0;i < this.slaves.length && i < this.slavesParents.length;i++) {
      this.slavesParents[i].append(this.slaves[i]);
    }
  },
  getCallArgsWithFilesForm: function () {
          var self = this;
          var form = $("<form method='post' style='display:none;' enctype='multipart/form-data'/>");
          $("body").append(form);
          _.each(this.params(), function (p) {
            if (p.sendAsParam()) {
              if (p.type() == "file") {
                self.attachFileParamsToForm(form, p);
              } else {
                form.append($("<input type='text'/>").attr("name", p.argName()).val(self.getParamValue(p)));
              }
            }
          });
          return form;
        },
  authorization: function () {
    return this.get("oauth").authorizationForRequests();
  },
  responseStatusCode: function () {return this.get("responseStatusCode");},
  details: function () {return this.get("details");},
  resultContent: function () {return this.get("resultContent");},
  getDetails: function (jqXHR) {
          return {
            "Status Code":
              "<span class='code " + ((jqXHR.status >= 400 || jqXHR.status == 0) ? "error" : "") + "'>" +
                jqXHR.status +
              "</span>" +
              " " + jqXHR.statusText,
            "Request URL": Scrive.serverUrl() + this.getCallUrl(),
            "Request Method": this.method(),
            "Authorisation needed": this.needsAuthorization() ? "Yes" : "No",
            "Date": jqXHR.getResponseHeader("Date")
          } ;
        },
  send: function (args) {
          var self = this;
          args = args || {};
          args.type = this.method();

          var form;
          if (!self.hasFileParams()) {
            args.data = this.getCallArgs();
          } else {
            var form = this.getCallArgsWithFilesForm();
            args.processData = false;
            args.contentType = false;
            args.data = new FormData(form[0]);
          }
          args.headers = {"Client-Name": "api-explorer",
                          "Client-Time": new Date().toISOString()
                        };
          if (this.needsAuthorization()) {
            args.headers.authorization = this.authorization();
          }
          args.cache = false;
          args.success = function (data, textStatus, jqXHR) {
            self.set("details", self.getDetails(jqXHR));
            self.set("resultContent", data);
            self.set("responseStatusCode", jqXHR.status);
            if (form != undefined) {
              self.detachFileParamsFromForm(form);
              form.remove();
            }
            self.trigger("send");
            setTimeout(function () { $(".response-result, .request-details").addClass("success"); }, 10);
            setTimeout(function () { $(".response-result, .request-details").removeClass("success"); }, 210);
          }
          args.error = function (jqXHR, textStatus, errorThrown) {
            self.set("details", self.getDetails(jqXHR));
            self.set("resultContent", jqXHR.responseText);
            self.set("responseStatusCode", jqXHR.status);
            if (form != undefined) {
              self.detachFileParamsFromForm(form);
              form.remove();
            }
            self.trigger("send");
            setTimeout(function () { $(".response-result, .request-details").addClass("error"); }, 10);
            setTimeout(function () { $(".response-result, .request-details").removeClass("error"); }, 210);
          }
          $.ajax(Scrive.serverUrl() + this.getCallUrl(), args);
        }
});

window.APICalls = new (Backbone.Model.extend({
  defaults: {
          calls: []
        },
  initialize: function (args) {
        },
  calls: function () {
    return this.get("calls");
  },
  authorization: function () {
          this.get("oauth");
        },
  apiV1Calls: function () {
          return _.filter(this.calls(), function (c) {
            return c.apiVersion() == "v1";
          })
        },
  apiV2Calls: function () {
          return _.filter(this.calls(), function (c) {
            return c.apiVersion() == "v2";
          })
        },
  registerNewCall: function (props, constructor) {
          props.constructor_ = constructor;
          this.calls().push(new AbstractAPICall(props));
        }
}))();

var APICall = function (props) {
  var static_prop_names = [
    "name",
    "apiVersion",
    "description",
    "sampleUrl",
    "method",
    "needsAuthorization",
    "params",
    "category",
    "tryToUseDocumentIDWithCopy",
    "expectBinaryResponse"
  ];
  var static_props = {};
  var dynamic_props = {};
  _.each(_.keys(props), function (k) {
    if (_.contains(static_prop_names, k)) {
      static_props[k] = props[k];
    } else {
      dynamic_props[k] = props[k];
    }
  });
  var constructor = function (instance_props) {
    return new (ApiCallInstance.extend(dynamic_props))($.extend(instance_props, static_props));
  };
  APICalls.registerNewCall(static_props, constructor);
}

window.APICallV1 = function (props) {
  props.apiVersion = "v1";
  return APICall(props);
}

window.APICallV2 = function (props) {
  props.apiVersion = "v2";
  return APICall(props);
}

})(window);
