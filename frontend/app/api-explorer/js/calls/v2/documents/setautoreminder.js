(function (window) {

new APICallV2({
  category: "signing",
  name: "Set Auto Reminder",
  description: "Set the number of days in which to send an automatic invitation reminder to the signatories waiting to sign.",
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
            description: "Including this parameter sets the number of days in\
                          which to send automatic reminders.\
                          Excluding it will remove automatic reminders from the document.",
            defaultValue: ""
          }),
          window.APIV2CallParamObjectVersion
        ]
});

})(window);
