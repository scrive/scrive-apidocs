(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory- Sign",
  description: "Sign a document.",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/sign",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/sign";
        },
  isInternal: true,
  needsAuthorization: true,
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamSignatoryID,
          new APICallParam({
            type: "json",
            argName: "fields",
            name: "Signatory Fields",
            description: "Updated information as entered by the signatory.",
            defaultValue: "[]"
          }),
          new APICallParam({
            type: "text",
            optional: true,
            argName: "authentication_type",
            name: "Authentication Type",
            description: "The authentication method to be used for signing.\
                          Not including this parameter will omit this check\
                          even though signing requires it.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            optional: true,
            argName: "authentication_value",
            name: "Authentication Value",
            description: "If the ‘authentication_type’ is not ‘standard’,\
                          then the value associated with it\
                          (e.g. phone number for ‘sms_pin’).",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            optional: true,
            argName: "sms_pin",
            name: "SMS PIN",
            description: "If the `authentication_method` for the signatory is\
                          `sms_pin` then SMS PIN also needs to be included.\
                          This is independant of other parameters in this call.",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "screenshots",
            name: "Screenshots (JSON)",
            sendAsParam: true,
            optional: true,
            description: "The screenshots and/or reference screenshots to use for the Evidence of Intent as part of the Evidence Package.",
            defaultValue: "{}"
          }),
          new APICallParam({
            type: "text",
            optional: false,
            argName: "accepted_author_attachments",
            name: "Accepted Author Attachments",
            description: "The list of author attachments which the signatory\
                          has seen and therefore accepted. The call will fail\
                          if not all author attachments with required set to\
                          “true” are included here.",
            defaultValue: "[]"
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
