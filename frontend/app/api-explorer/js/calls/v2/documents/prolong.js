(function (window) {

new APICallV2({
  category: "signing",
  name: "Prolong",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/prolong",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/prolong";
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "document_id",
            name: "$document_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "days",
            name: "Days",
            description: "TODO",
            defaultValue: ""
          }),
          window.APICallParamObjectVersion
        ]
});

})(window);
