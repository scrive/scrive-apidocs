
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
      var json = JSON.parse(content);
      return JSON.stringify(JSON.parse(content), undefined, 2);
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
    })
    return res;
  },
  render: function () {
    var self = this;
    var model = this.model;
    var call = model.selectedApiCall();
    var panel = $(this.el);
    var panelBody = $("<div class='panel-body'></div>")
    panel.empty().append(panelBody);

    if (call.needsAuthorization() && call.responseStatusCode() == 403 && !model.oauth().isSetUp()) {
      var buttonRow = $("<div class='row'>");
      var buttonCol = $("<div class='col-xs-4 col-xs-offset-8'>");
      var button = $("<button class='btn btn-block btn-success' type='button'>Authorize</button>").click(function () {
        model.oauth().clear();
        model.setMode("authorize");
        window.location.hash = "#" + model.oauth().mode();

      });
      panelBody.append(buttonRow.append(buttonCol.append(button)));
    }

    panelBody.append("<p><strong>Request details</strong></p>")
    panelBody.append($("<div class='request-details well well-sm'></div>")
      .append($("<code class='language-request'/>").html(this.niceDetails(call.details())))
    );

    panelBody.append("<p><strong>Response body</strong></p>")
    if (call.resultContent()) {
      var copyButton = $("<div class='btn btn-copy'>Copy</div>");
      copyButton.attr("data-clipboard-text", self.niceContent(call.resultContent()));
      copyButton.tooltip({title: "Copy to clipoard", trigger: "hover", placement: "top", animation: false});
      panelBody.append(copyButton);
    }
    panelBody.append($("<div class='response-result well well-sm'></div>")
      .append($("<code class='language-javascript'/>").text(this.niceContent(call.resultContent())))
    );
    Prism.highlightAll(panelBody);

    var copyClient = new ZeroClipboard(copyButton);

    copyClient.on("aftercopy", function (event) {
      copyButton.tooltip("destroy");
      copyButton.tooltip({title: "Copied!", trigger: "hover", placement: "top", animation: false});
      copyButton.tooltip("show");
      copyButton.on("hidden.bs.tooltip", function () {
        copyButton.off("hidden.bs.tooltip");
        copyButton.tooltip("destroy");
        copyButton.tooltip({title: "Copy to clipoard", trigger: "hover", placement: "top", animation: false});
      })

      if (call.tryToUseDocumentIDWithCopy()) {
        try {
          var json = JSON.parse(call.resultContent());
          if (json.id != undefined) {
            LocalStorage.set("param", "documentid", json.id);
          }
        } catch (e) { }
      }
    })

    return this;
  }
});

})(window);
