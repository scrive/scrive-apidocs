
(function (window) {

new APICallV1({
  category: ["draft", "main"],
  name: "Create from template",
  description: "Creates document based on existing template",
  sampleUrl: "/api/v1/createfromtemplate/$templateid$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/createfromtemplate/" + this.get("templateid");
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "templateid",
            name: "Template id",
            sendAsParam: false,
            description: "Id of template that should be used to create new document",
            defaultValue: ""
          })
        ]
});

})(window);
