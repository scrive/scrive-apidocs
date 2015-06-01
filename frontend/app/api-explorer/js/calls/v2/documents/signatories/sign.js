(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory- Sign",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/sign",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/sign";
        },
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          new APICallParam({
            type: "json",
            argName: "fields",
            name: "Signatory Fields",
            description: "TODO",
            defaultValue: "[]"
          }),
          new APICallParam({
            type: "json",
            argName: "screenshots",
            name: "Screenshots (JSON)",
            sendAsParam: true,
            optional: true,
            description: "TODO",
            defaultValue: "{}"
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
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
