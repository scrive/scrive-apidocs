
(function (window) {

new APICallV1({
  category: "draft",
  name: "Extract texts",
  description: "Extract texts API call",
  sampleUrl: "/api/v1/extracttexts/$docid$/$fileid$",
  method: "GET",
  getCallUrl: function () {
          return "/api/v1/extracttexts/" + this.get("documentid")  + "/" + this.get("fileid") ;
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
          }),
          new APICallParam({
            type: "text",
            argName: "fileid",
            name: "File id",
            sendAsParam: false,
            description: "",
            defaultValue: ""
          }),
          new APICallParam({
            type: "json",
            argName: "json",
            name: "JSON",
            sendAsParam: true,
            description: "",
            defaultValue: "{\"rects\": [{\"rect\":[0,0,1,1],\"page\": 1}]}"
          })
        ]
});

})(window);
