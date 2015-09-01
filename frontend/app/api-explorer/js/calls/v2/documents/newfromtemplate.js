(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "New from template",
  description: "Create a new document from a template ID.",
  sampleUrl: "/api/v2/documents/newfromtemplate/$template_id$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/newfromtemplate/" + this.get("template_id");
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {
    'v1': 'Create from template'
  },
  params: [
          new APICallParam({
            type: "text",
            argName: "template_id",
            name: "$template_id$",
            sendAsParam: false,
            description: "ID of document to use as template.\
                         The document must be a template.",
            defaultValue: ""
          })
          , window.APIV2CallParamObjectVersion
        ]
});

})(window);
