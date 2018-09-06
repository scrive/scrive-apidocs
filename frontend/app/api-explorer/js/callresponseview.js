
(function (window) {

window.CallResponseView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render");
    this.model.selectedApiCall().bind("send", this.render);
    this.render();
  },
  destroy: function () {
    this.model.selectedApiCall().unbind("send", this.render);
  },
  arrayBufferToB64: function (ab) {
    var arr = new Uint8Array(ab);
    var length = arr.length;
    var CHUNK_SIZE = 32768;
    var index = 0;
    var raw = "";
    var slice;
    while (index < length) {
      slice = arr.slice(index, Math.min(index + CHUNK_SIZE, length));
      raw += String.fromCharCode.apply(null, slice);
      index += CHUNK_SIZE;
    }
    return btoa(raw);
  },
  niceContent: function (content) {
    if (!content) {
      return "";
    }
    if (this.model.selectedApiCall().expectBinaryResponse() && content.length > 1000) {
      return content.substring(0, 500) +
             "\n...\n...\n...\n...\n...\n...\n" +
             content.substring(content.length - 500, content.length);
    }
    try {
      var json;
      if (typeof content == "object") {
        json = content;
      } else {
        json = JSON.parse(content);
      }
      return JSON.stringify(json, undefined, 2);
    } catch (e) {
      return content;
    }
  },
  niceDetails: function (details) {
    if (!details) {
      return "";
    }
    var res = "";
    _.each(_.keys(details), function (k) {
      res += "<span class='header'>" + k +  ": </span>" + "<span class='detail'>" + details[k] + "</span>" + "\n";
    });
    return res;
  },
  render: function () {
    var self = this;
    var model = this.model;
    var call = model.selectedApiCall();
    var callPrototype = call.callPrototype();
    var panel = $(this.el);
    var panelBody = $("<div class='panel-body'></div>");
    panel.empty().append(panelBody);

    if (call.needsAuthorization() && call.responseStatusCode() == 403 && !model.oauth().isSetUp()) {
      var buttonRow = $("<div class='row'>");
      var buttonCol = $("<div class='col-xs-4 col-xs-offset-8'>");
      var button = $("<button class='btn btn-block btn-success' type='button'>Authorise</button>").click(function () {
        model.oauth().clear();
        model.setMode("authorize");
        window.location.hash = "#" + model.oauth().mode();

      });
      panelBody.append(buttonRow.append(buttonCol.append(button)));
    }

    panelBody.append("<p><strong>Request details</strong></p>");
    panelBody.append($("<div class='request-details well well-sm'></div>")
      .append($("<code class='language-request'/>").html(this.niceDetails(call.details())))
    );

    if ((call.resultContentType() === "application/json; charset=UTF-8" ||
         call.resultContentType() === "text/plain; charset=UTF-8")) {
        panelBody.append("<p><strong>Response body</strong></p>");
        if (call.resultContent()) {
          var copyButton = $("<div class='btn btn-copy'>Copy</div>");
          copyButton.attr("data-clipboard-text", self.niceContent(call.resultContent()));
          copyButton.tooltip({title: "Copy to clipoard", trigger: "hover", placement: "top", animation: false});
          panelBody.append(copyButton);
        }
        panelBody.append($("<div class='response-result well well-sm'></div>")
          .append($("<code class='language-javascript'/>").text(this.niceContent(call.resultContent())))
        );
        if (call.resultContentLength() < 102400) {
          Prism.highlightAll(panelBody);
        }

        var copyClient = new Clipboard(".btn-copy");

        copyClient.on("success", function (event) {
          var copyButton = $(".btn-copy");
          copyButton.tooltip("destroy");
          copyButton.tooltip({title: "Copied!", trigger: "hover", placement: "top", animation: false});
          copyButton.tooltip("show");
          copyButton.on("hidden.bs.tooltip", function () {
            copyButton.off("hidden.bs.tooltip");
            copyButton.tooltip("destroy");
            copyButton.tooltip({title: "Copy to clipoard", trigger: "hover", placement: "top", animation: false});
          });

          if (callPrototype.tryToUseDocumentIDWithCopy()) {
            try {
              var json;
              var result = call.resultContent();
              if (typeof result === "string") {
                json = JSON.parse(result);
              } else {
                json = result;
              }
              if (json.id != undefined) {
                LocalStorage.set("param", "documentid", json.id);
                LocalStorage.set("param", "document_id", json.id);
              }
            } catch (e) { }
          }
        });
    } else if (call.resultContentType() === "image/png") {
      var img = $("<img class='response-image-result'/>");
      img[0].src = "data:image/png;base64," + this.arrayBufferToB64(call.resultContent());
      panelBody.append($("<div class='response-result well well-sm'></div>").append(img));
    } else if (call.resultContentType() === "application/pdf") {
      var iframe = $("<iframe width='100%' height='100%' frameborder='0' style='border: 0;' />");
      iframe[0].src = "data:application/pdf;base64," + this.arrayBufferToB64(call.resultContent());
      panelBody.append($("<div class='response-result well well-sm'></div>").append(iframe));
    }
    return this;
  }
});

})(window);
