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
          window.APIV2CallParamDocumentID,
          new APICallParam({
            type: "text",
            argName: "days",
            name: "Days",
            optional: true,
            description: "TODO",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
