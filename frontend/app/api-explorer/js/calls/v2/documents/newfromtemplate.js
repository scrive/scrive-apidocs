(function (window) {

new APICallV2({
  category: ["draft", "main"],
  name: "New from template",
  description: "TODO",
  sampleUrl: "/api/v2/documents/newfromtemplate/$template_id$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/newfromtemplate/" + this.get("template_id");
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "template_id",
            name: "$template_id$",
            sendAsParam: false,
            description: "Id of document to use as template.\
                         The document must be a template.",
            defaultValue: ""
          })
          , window.APICallParamObjectVersion
        ]
});

})(window);
