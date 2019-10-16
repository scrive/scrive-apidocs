(function (window) {

new APICallV2({
  category: ["draft"],
  name: "Start with portal",
  description: "Start the signing process for multiple documents in preparation with portal delivery.",
  sampleUrl: "/api/v2/documents/startwithportal",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/startwithportal";
        },
  needsAuthorization: true,
  params: [
    new APICallParam({
      type: "json",
      argName: "document_ids",
      optional: false,
      name: "Document IDs",
      sendAsParam: true,
      description: "IDs of the documents to start.",
      defaultValue: "[]"
    })
  ]
});

})(window);
