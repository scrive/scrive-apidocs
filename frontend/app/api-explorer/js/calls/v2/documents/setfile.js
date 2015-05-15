(function (window) {

new APICallV2({
  category: "draft",
  name: "Set File",
  description: "TODO",
  sampleUrl: "/api/v2/documents/$document_id$/setfile",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/documents/" + this.get("document_id") + "/setfile";
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
            type: "file",
            argName: function () { return "file";},
            name: "file (application/pdf)",
            sendAsParam: true,
            optional: true,
            description: "PDF to be used as main file for the document.",
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
          })
          , window.APICallParamObjectVersion
        ]
});

})(window);
