var React = require("react");
var util = require("../../util");
var $ = require("jquery");

var MaskedPersonalNumber = require(
  "../../../scripts/signview/identify/masked_personal_number"
);

describe("MaskedPersonalNumber", function () {
  var $container = null;

  var renderComponent = function (props) {
    props = props || {};

    $container = $("<div />");

    return React.render(
      React.createElement(
        MaskedPersonalNumber, props
      ),
      $container[0]
    );
  };

  afterEach(function () {
    if ($container) {
      console.log(React.unmountComponentAtNode($container[0]));
    }
  });

  it("should not render any text if neither number nor placeholder are specified", function () {
    var component = renderComponent({});
    assert.equal(React.findDOMNode(component).innerText, "");
  });

  it("should not render placeholder text if number isn't specified", function () {
    var component = renderComponent({placeholder: "HERE NUMBER BE"});
    assert.equal(React.findDOMNode(component).innerText, "HERE NUMBER BE");
  });

  it("should mask 10-digit Swedish BankID number without hyphen", function () {
    var component = renderComponent({number: "7001011234"});
    assert.equal(React.findDOMNode(component).innerText, "700101****");
  });

  it("should mask 10-digit Swedish BankID number with hyphen", function () {
    var component = renderComponent({number: "700101-1234"});
    assert.equal(React.findDOMNode(component).innerText, "700101-****");
  });

  it("should mask 12-digit Swedish BankID number without hyphen", function () {
    var component = renderComponent({number: "197001011234"});
    assert.equal(React.findDOMNode(component).innerText, "19700101****");
  });

  it("should mask 12-digit Swedish BankID number with hyphen", function () {
    var component = renderComponent({number: "19700101-1234"});
    assert.equal(React.findDOMNode(component).innerText, "19700101-****");
  });

  it("should mask Norwegian BankID number without hyphen", function () {
    var component = renderComponent({
      number: "70010112345",
      isNorwegian: true,
      isDanishPersonal: false,
      isDanishEmployee: false,
      isFinnish: false
    });

    assert.equal(React.findDOMNode(component).innerText, "700101*****");
  });

  it("should mask Norwegian BankID number with hyphen", function () {
    var component = renderComponent({
      number: "700101-12345",
      isNorwegian: true,
      isDanishPersonal: false,
      isDanishEmployee: false,
      isFinnish: false
    });

    assert.equal(React.findDOMNode(component).innerText, "700101-*****");
  });

  it("should mask Danish NemID number without hyphen", function () {
    var component = renderComponent({
      number: "0101701234",
      isNorwegian: false,
      isDanishPersonal: true,
      isDanishEmployee: false,
      isFinnish: false
    });

    assert.equal(React.findDOMNode(component).innerText, "010170****");
  });

  it("should mask Danish BankID number with hyphen", function () {
    var component = renderComponent({
      number: "010170-1234",
      isNorwegian: false,
      isDanishPersonal: true,
      isDanishEmployee: false,
      isFinnish: false
    });

    assert.equal(React.findDOMNode(component).innerText, "010170-****");
  });

  it("should mask Danish BankID number without hyphen", function () {
    var component = renderComponent({
      number: "30808460",
      isNorwegian: false,
      isDanishPersonal: false,
      isDanishEmployee: true,
      isFinnish: false
    });

    assert.equal(React.findDOMNode(component).innerText, "3080****");
  });


  it("should mask Finnish ID number", function () {
    var component = renderComponent({
      number: "010170+123X",
      isNorwegian: false,
      isDanishPersonal: false,
      isDanishEmployee: false,
      isFinnish: true
    });

    assert.equal(React.findDOMNode(component).innerText, "010170+****");
  });
});
