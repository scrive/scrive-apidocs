(function (window) {

new APICallV2({
  category: "signing",
  name: "Signatory: Set Attachment",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/$signatory_id$/setattachment",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/"
                                      + this.get("signatory_id")
                                      + "/setattachment";
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
            argName: "name",
            name: "Attachment name",
            sendAsParam: true,
            description: "TODO",
            defaultValue: ""
          }),
          new APICallParam({
            type: "file",
            argName: function () { return "file";},
            name: "attachment (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "TODO",
            limit: 1,
            defaultValue: function (self) {
              var input = $("<input type='file' class='form-control multiFileInput'/>");
              input.MultiFile({
                afterFileAppend: function (input, title, fileinput) {
                  setTimeout(function () {self.trigger("files-changed");}, 10);
                }
              });
              return input.data("MultiFile");
            }
          }),
          window.APICallParamObjectVersion
        ]
});

})(window);
