(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory- Check",
  description: "Check if a signatory `Sign` call could succeed.",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/check",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/check";
        },
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
