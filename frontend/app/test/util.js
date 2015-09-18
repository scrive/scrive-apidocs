define(["legacy_code", "React"], function(legacy_code, React) {
  var exports = {};

  var TestUtils = React.addons.TestUtils;

  var taskContextContainer = exports.taskContextContainer = function (comp, props) {
    return React.createElement(React.createClass({
      childContextTypes: {
        addTask: React.PropTypes.func.isRequired,
        removeTask: React.PropTypes.func.isRequired
      },

      getChildContext: function () {
        return {
          addTask: function (task) { },
          removeTask: function (task) { }
        };
      },

      render: function () {
        props.ref = "comp";
        return React.createElement(comp, props);
      }
    }));
  };

  var createDocument = exports.createDocument = function (cb) {
    var doc = new Document({ id: 0 });
    doc.fetch({ processData: true, cache: false });
    doc.on("change:ready", function () {
      doc.mainfile().fetch({ processData: true, cache: false });
      doc.mainfile().on("change:pages", function () {
        cb(doc);
      });
    });
  };

  var waitUntil = exports.waitUntil = function (fn, cb) {
    if (fn()) {
      return cb();
    }

    setTimeout(function () {
      waitUntil(fn, cb);
    }, 10);
  };

  var createDesignView = exports.createDesignView = function (cb) {
    createDocument(function(doc) {
      var designView = new DesignViewModel({
        document : doc
      });
      cb(designView);
    })
  };

  exports.addPlacement = function (doc, id, part, fieldOptions) {
    part = part || 0;
    id = id || +new Date();
    var sigs = doc.signatories();
    var file = doc.mainfile();
    var page = file.pages()[0];
    var sig = sigs[part];
    var options = {
      signatory: sig
      , type: "custom"
      , name: "field-" + id
      , obligatory: true
      , shouldbefilledbysender: sig.author()
    };
    var field = new Field(_.extend(options, fieldOptions));
    field.addedByMe = true;
    sig.addField(field);
    var placement = new FieldPlacement({
      field: field
    });
    placement.view = new Backbone.View;
    placement.set({ page: 1 });
    field.addPlacement(placement);
    if (page) {
      page.addPlacement(placement);
    }
    return placement;
  };

  exports.cleanTimeoutsAndBody = function() {
    var id = window.setTimeout(function() {}, 0);
    while (id--) {
      window.clearTimeout(id);
    }
    $('body').empty();
  };

  exports.withSmallScreen = function (fn) {
    var oldBrowserInfo = window.BrowserInfo;
    window.BrowserInfo = {
      isSmallScreen: function () { return true; },
      isPadDevice: function () { true; }
    };

    var done = function () {
      window.BrowserInfo = oldBrowserInfo;
    };

    if (fn.length === 0) {
      fn();
      done();
    } else {
      fn(done);
    }
  };

  exports.findComponents = function (tree, comp) {
    return TestUtils.findAllInRenderedTree(tree, function (node) {
      return TestUtils.isCompositeComponentWithType(node, comp);
    });
  };

  return exports;
});
