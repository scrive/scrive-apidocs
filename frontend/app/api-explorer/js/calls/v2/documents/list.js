(function (window) {

new APICallV2({
  category: "fetch",
  name: "List",
  description: "Fetch a list of documents, with sorting and filtering options",
  sampleUrl: "/api/v2/documents/list",
  method: "GET",
  getCallUrl: function () {
          return "/api/v2/documents/list";
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "offset",
            name: "Offset",
            sendAsParam: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "max",
            optional: true,
            name: "Max",
            sendAsParam: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "sorting",
            optional: true,
            name: "Sorting",
            sendAsParam: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "filter",
            optional: true,
            name: "Filter",
            sendAsParam: true,
            description: "TODO",
            defaultValue: ""
          })
        ]
});

})(window);
