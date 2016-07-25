var React = require("react");
var $ = require("jquery");

var Document = require("../documents.js").Document;
var LoadingDialog = require("../loading.js").LoadingDialog;

var AuthorViewComponent = require("../../scripts/authorview/authorview");

module.exports.AuthorView = function(args) {
  var mainDiv = $("<div/>");
  var version = 0;
  var document_ = null;
  var view = null;
  var self = this;

  this.el = function () {
    return mainDiv;
  };

  this.reload = function (force) {
    var oldDocument = document_;
    var oldView = view;

    if ((!oldView.ready() || $(".modal.active").size() > 0) && (!force)) {
      return;
    }

    if (force) {
      LoadingDialog.open();
    }

    version++;
    var reloadversion = version;

    view.stopRefreshingHistory();

    var newDiv = $("<div/>");
    var newDocument = makeDocument();
    var newView = renderComponent(newDocument, newDiv);

    var connectNewView = function () {
      if (newView.ready()) {
        if (reloadversion == version) {
          newView.setCurrentSignatoryIndex(oldView.currentSignatoryIndex());
          newView.setIsHistoryViewExpanded(oldView.isHistoryViewExpanded());

          React.unmountComponentAtNode(mainDiv[0]);
          mainDiv.empty();
          mainDiv.replaceWith(newDiv);

          document_ = newDocument;
          view = newView;
          mainDiv = newDiv;

          if (force) {
            LoadingDialog.close();
          }
        } else {
          React.unmountComponentAtNode(newDiv[0]);
        }
      } else {
        window.setTimeout(connectNewView, 500);
      }
    };
    connectNewView();
  };

  var makeDocument = function () {
    return new Document({
      id : args.id,
      viewer: args.viewer,
      evidenceAttachments: true
    });
  };

  var renderComponent = function (doc, container) {
    return React.render(
      React.createElement(
        AuthorViewComponent, {document: doc, onReload: self.reload}
      ),
      container[0]
    );
  };

  document_ = makeDocument()
  view = renderComponent(document_, mainDiv);
};
