(function (window) {

new APICallV2({
  category: "other",
  name: "Delete",
  description: "Delete a document that is in Trash.",
  sampleUrl: "/api/v2/documents/$document_id$/delete",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/delete";
        },
  needsAuthorization: true,
  equivalentCalls: {
    'v1': 'Delete'
  },
  params: [
          window.APIV2CallParamDocumentID,
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
