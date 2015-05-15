(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory: Reject",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/reject",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/reject";
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
            argName: "signatory_id",
            name: "$signatory_id$",
            sendAsParam: false,
            useLocalStorage: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "reason",
            name: "Reject reason",
            description: "TODO",
            defaultValue: ""
          }),
          window.APICallParamObjectVersion
        ]
});

})(window);
