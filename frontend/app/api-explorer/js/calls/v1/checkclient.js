
(function (window) {

new APICallV1({
  name: "Check client",
  description: "Check if your current api client is still supported",
  sampleUrl: "/api/v1/checkclient",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/checkclient";
        },
  params: [
          new APICallParam({
            type: "json",
            argName: "client",
            name: "Client",
            description: "Description of API client that is supposed to be checked",
            defaultValue: JSON.stringify({
              platform: "iOS",
              platform_version:"5.0",
              client:"Scrive iPad App",
              client_version:"0.1"
            })
          })
        ]
});

})(window);
