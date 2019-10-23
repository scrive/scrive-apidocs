(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "New from template",
  description: "Create a new document given the document ID for a document that is a template.",
  sampleUrl: "/api/v2/documents/newfromtemplate/$document_id$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/newfromtemplate/" + this.get("document_id");
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  equivalentCalls: {
    'v1': 'Create from template'
  },
  params: [
          window.APIV2CallParamDocumentID
          , window.APIV2CallParamObjectVersion
          , new APICallParam({
              type: "text",
              argName: "folder_id",
              sendAsParam: true,
              optional: true,
              description: "ID of the folder to save the document to"
          })
        ]
});

})(window);
