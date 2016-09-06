var React = require("react");
var util = require("../util")
var $ = require("jquery");

var HtmlTextWithSubstitution = require("../../scripts/common/htmltextwithsubstitution");

describe("common/htmltextwithsubstitution", function () {
  var renderComponent = function (props) {
    props = props || {};

    return React.render(
      React.createElement(
        HtmlTextWithSubstitution, props
      ),
      $("body")[0]
    );
  };

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should not alter the text if no substitutions are specified", function () {
    var component = renderComponent({
      secureText: "HERE SECURE TEXT BE"
    });

    var componentEl = $(React.findDOMNode(component));
    assert.equal(componentEl.html(), "HERE SECURE TEXT BE");
  });

  it("should keep HTML tags in secure text", function () {
    var component = renderComponent({
      secureText: "<span id='secure'>HERE SECURE TEXT BE</span>"
    });

    var secureTextNode = $("span#secure", $(React.findDOMNode(component)));
    assert.lengthOf(secureTextNode, 1);
  });

  it("should handle text substitution and HTML escaping", function () {
    var component = renderComponent({
      secureText: "<span class='text'></span> <span class='escaped'></span>",
      subs: {
        ".text": "substituted",
        ".escaped": "<strong>escaped</strong>"
      }
    });

    var componentEl = $(React.findDOMNode(component));

    assert.equal($("span.text", componentEl).html(), "substituted");
    assert.equal(
      $("span.escaped", componentEl).html(), "&lt;strong&gt;escaped&lt;/strong&gt;"
    );
  });

  it("should handle list substitution with one item", function () {
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["first"]
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.equal($("span.list", componentEl).html(), "<span>first</span>");
  });

  it("should escape HTML in list items", function () {
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["<b>first</b>"]
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.equal($("span.list", componentEl).html(), "<span>&lt;b&gt;first&lt;/b&gt;</span>");
  });

  it("should handle custom list item wrapper", function () {
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["first"],
          wrapper: "<strong />"
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.equal($("span.list", componentEl).html(), "<strong>first</strong>");
  });

  it("should handle list substitution with two items", function () {
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["first", "second"]
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.lengthOf($("span.list > span", componentEl), 3);
    assert.equal($("span.list > span:nth-child(1)", componentEl).html(), "first");
    assert.equal($("span.list > span:nth-child(2)", componentEl).html(), " and ");
    assert.equal($("span.list > span:nth-child(3)", componentEl).html(), "second");
  });

  it("should handle list substitution with three items", function () {
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["first", "second", "third"]
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.lengthOf($("span.list > span", componentEl), 5);
    assert.equal($("span.list > span:first-child", componentEl).html(), "first");
    assert.equal($("span.list > span:nth-child(2)", componentEl).html(), ", ");
    assert.equal($("span.list > span:nth-child(3)", componentEl).html(), "second");
    assert.equal($("span.list > span:nth-child(4)", componentEl).html(), " and ");
    assert.equal($("span.list > span:nth-child(5)", componentEl).html(), "third");
  });

  it("should handle list substitution with custom separators", function () {
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["first", "second", "third"],
          separator: " or ",
          lastSeparator: "optionally"
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.lengthOf($("span.list > span", componentEl), 5);
    assert.equal($("span.list > span:first-child", componentEl).html(), "first");
    assert.equal($("span.list > span:nth-child(2)", componentEl).html(), " or ");
    assert.equal($("span.list > span:nth-child(3)", componentEl).html(), "second");
    assert.equal($("span.list > span:nth-child(4)", componentEl).html(), " optionally ");
    assert.equal($("span.list > span:nth-child(5)", componentEl).html(), "third");
  });

  it("should handle links", function () {
    var component = renderComponent({
      secureText: "<a class='link'>Click me!</span>",
      links: {
        ".link": "http://www.example.com/"
      }
    });

    var componentEl = $(React.findDOMNode(component));

    assert.equal($("a.link", componentEl).attr("href"), "http://www.example.com/");
    assert.equal($("a.link", componentEl).attr("target"), "_blank");
    assert.equal($("a.link", componentEl).attr("rel"), "noopener noreferrer");
  });

  it("should handle additional classes", function () {
    var component = renderComponent({
      secureText: "<span class='secure'>HERE SECURE TEXT BE</span>",
      classes: {
        ".secure": "someClass"
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.lengthOf($("span.secure.someClass", componentEl), 1);
  });

  it("should handle list substitution with four items", function () {
    // This is a test for the bug fixed in #326.
    var component = renderComponent({
      secureText: "<span class='list'></span>",
      lists: {
        ".list": {
          items: ["first", "second", "third", "fourth"]
        }
      }
    });

    var componentEl = $(React.findDOMNode(component));
    assert.equal($("span.list").text(), "first, second, third and fourth");
  });
});
