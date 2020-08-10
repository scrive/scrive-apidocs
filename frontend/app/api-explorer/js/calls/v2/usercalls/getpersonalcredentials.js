(function (window) {

new APICallV2({
  category: "v2-user-related",
  name: "Get token for personal credentials",
  description: "Get the login token, QR code, etc. for personal credentials",
  sampleUrl: "/api/v2/gettokenforpersonalcredentials/$user_id$",
  method: "POST",
  getCallUrl: function () {
          return "/api/v2/gettokenforpersonalcredentials/" + this.get("user_id");
        },
  needsAuthorization: true,
  tryToUseDocumentIDWithCopy: true,
  params: [
        new APICallParam({
                type: "text",
                argName: "user_id",
                name: "$user_id$",
                sendAsParam: false,
                useLocalStorage: true,
                description: "The user ID",
                defaultValue: ""
                })
        ]
});

})(window);
