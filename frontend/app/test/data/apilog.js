module.exports = {
  call_logs: [
    {
      id: "1",
      time: "2017-10-13T08:41:16Z",
      data: {
        response: {
          body: "{\"spam\": true}",
          code: 200
        },
        request: {
          params_get: {
            spam: "eggs"
          },
          params_post: {
            eggs: "spam"
          },
          uri: "/api/v2/documents/99/get",
          method: "POST"
        }
      }
    },
    {
      id: "2",
      time: "2017-10-13T08:41:16Z",
      data: {
        response: {
          body: "{\"spam\": false}",
          code: 404
        },
        request: {
          params_get: {
            spam: "eggs"
          },
          params_post: {
            eggs: "spam"
          },
          uri: "/api/v2/documents/99/get",
          method: "GET"
        }
      }
    }
  ]
};
