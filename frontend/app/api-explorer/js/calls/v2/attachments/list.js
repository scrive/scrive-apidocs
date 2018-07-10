(function (window) {

new APICallV2({
  category: "attachment",
  name: "List attachments",
  description: "List all available attachments.",
  sampleUrl: "/api/v2/attachments/list",
  method: "GET",
  getCallUrl: function () {
    return "/api/v2/attachments/list";
  },
  needsAuthorization: true,
  params: [
    new APICallParam({
      type: "text",
      argName: "domain",
      optional: true,
      name: "Domain",
      sendAsParam: true,
      description: "Set to 'All' to return all attachments. Otherwise, it will return the attachments of the current user.",
      defaultValue: "All"
    }),
    new APICallParam({
      type: "json",
      argName: "sorting",
      optional: true,
      name: "Sorting",
      sendAsParam: true,
      description: "Sorting options.",
      defaultValue: "[{\"sort_by\":\"time\",\"order\":\"descending\"}]"
    }),
    new APICallParam({
      type: "json",
      argName: "filter",
      optional: true,
      name: "Filter",
      sendAsParam: true,
      description: "Filtering options.",
      defaultValue: "[]"
    })
  ]
});

})(window);
