(function (window) {

new APICallV2({
  category: "fetch",
  name: "Available",
  description: "Given a list of document IDs, checks which ones are available for the user. This call silently ignores any bad document IDs (whether due to lacking permissions, document not existing, or any other reason). Return value may be an empty list.",
  sampleUrl: "/api/v2/documents/available",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/available";
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "json",
            argName: "ids",
            name: "Document IDs",
            sendAsParam: true,
            description: "JSON Array (list) of document IDs to check",
            defaultValue: "[]"
          })
        ]
});

})(window);
