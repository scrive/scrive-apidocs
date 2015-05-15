(function (window) {

new APICallV2({
  category: "signing",
  name: "Set Auto Reminder",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/setautoreminder",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/setautoreminder";
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
