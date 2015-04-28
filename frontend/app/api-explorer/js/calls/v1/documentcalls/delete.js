
(function (window) {

new APICallV1({
  category: "signing",
  name: "Delete",
  description: "Move selected document to trash",
  sampleUrl: "/api/v1/delete/$documentid$",
  method: "DELETE",
  getCallUrl: function () {
          return "/api/v1/delete/" + this.get("documentid");
        },
  needsAuthorization: true,
  params: [
          new APICallParam({
            type: "text",
            argName: "documentid",
            name: "Document id",
            sendAsParam: false,
            useLocalStorage: true,
            description: "",
            defaultValue: ""
          })
        ]
});

})(window);
