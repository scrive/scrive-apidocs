(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory: Check",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/check",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/check";
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "document_id",
            name: "$document_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "signatory_id",
            name: "$signatory_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            optional: true,
            argName: "authentication_type",
            name: "Authentication Type",
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            optional: true,
            argName: "authentication_value",
            name: "Authentication Value",
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            optional: true,
            argName: "sms_pin",
            name: "SMS PIN",
            description: "TODO",
            defaultValue: ""
          }),
          window.APICallParamObjectVersion
        ]
});

})(window);
