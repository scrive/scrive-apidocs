
(function (window) {

new APICallV1({
  category: "other",
  name: "Signup",
  description: "Signup new user, and send invitation email to them",
  sampleUrl: "/api/v1/signup",
  method: "POST",
  getCallUrl: function () {
          return "/api/v1/signup";
        },
  needsAuthorization: false,
  params: [
          new APICallParam({
            type: "text",
            argName: "email",
            name: "Email",
            description: "Email that will identify user",
            defaultValue: ""
          }),
          new APICallParam({
            type: "text",
            argName: "lang",
            name: "Language",
            description: "Optional. ISO code of language for new user",
            defaultValue: "en"
          })
        ]
});

})(window);
