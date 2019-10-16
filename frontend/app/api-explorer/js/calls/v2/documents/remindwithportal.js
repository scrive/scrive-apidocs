(function (window) {

new APICallV2({
  category: ["signing"],
  name: "Remind with portal",
  description: "Send a reminder for multiple pending documents with portal delivery.",
  sampleUrl: "/api/v2/documents/startwithportal",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/remindwithportal";
        },
  needsAuthorization: true,
  params: [
    new APICallParam({
      type: "json",
      argName: "document_ids",
      optional: false,
      name: "Document IDs",
      sendAsParam: true,
      description: "IDs of the documents to send a reminder about.",
      defaultValue: "[]"
    })
  ]
});

})(window);
