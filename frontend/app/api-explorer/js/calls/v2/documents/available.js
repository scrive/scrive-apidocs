(function (window) {

new APICallV2({
  category: "fetch",
  name: "Available",
  description: "TODO",
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
            name: "Document Ids",
            sendAsParam: true,
            description: "TODO",
            defaultValue: "[]"
          })
        ]
});

})(window);
