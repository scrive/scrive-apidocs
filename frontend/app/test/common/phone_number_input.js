var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var util = require("../util");

var PhoneNumberInput = require("../../scripts/common/phone_number_input");

describe("common/phone_number_input", function () {
  var container = null;

  beforeEach(function () {
    sinon.stub(document, "addEventListener");
    sinon.stub(document, "removeEventListener");
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    document.addEventListener.restore();
    document.removeEventListener.restore();

    util.cleanTimeoutsAndBody();
  });

  describe("PhoneNumberInput", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          className: "",
          countries: undefined,
          infotext: "Phone Number",
          readonly: false,
          value: "",
          onChange: sinon.stub(),
          onRemove: sinon.stub(),
          onTab: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(PhoneNumberInput.PhoneNumberInput, actualProps),
        container
      );

      return component;
    };

    it("should initialize state", function () {
      var component = renderComponent();
      assert.equal(component.state.countryCode, "unknown");
      assert.isFalse(component.state.expanded);
      assert.isFalse(component.state.focus);
    });

    describe("componentDidMount", function () {
      it("should start listening to document click events", function () {
        var component = renderComponent();
        assert.isTrue(document.addEventListener.calledWith(
          "click", component.onDocumentClick
        ));
      });

      it("should reset the value to default if it's empty", function () {
        var component = renderComponent();
        assert.isTrue(component.props.onChange.calledWith("+46"));
      });

      it("should not reset the value if isn't empty", function () {
        var component = renderComponent({value: "+48123456789"});
        assert.isFalse(component.props.onChange.called);
      });
    });

    it("should stop listening to document click events when it unmounts", function () {
      var component = renderComponent();

      React.unmountComponentAtNode(container);
      container = null;

      assert.isTrue(document.removeEventListener.calledWith(
        "click", component.onDocumentClick
      ));
    });

    it("should set new country code when value changes", function () {
      var component = renderComponent();
      sinon.stub(component, "guessSelectedCountry").returns({iso2: "pl"});

      component.componentWillReceiveProps({value: "+48123456789"});
      assert.isTrue(component.guessSelectedCountry.calledWith("+48123456789"));
      assert.equal(component.state.countryCode, "pl");
    });

    describe("componentDidUpdate", function () {
      it("should reset value to default when it becomes empty", function () {
        var component = renderComponent();

        component.componentDidUpdate({value: "+48123456789"});
        assert.isTrue(component.props.onChange.calledWith("+46"));
      });

      it("should prepend the default country code if new value doesn't start with +", function () {
        var component = renderComponent({value: "123456789"});

        component.componentDidUpdate({value: ""});
        assert.isTrue(component.props.onChange.calledWith("+46123456789"));
      });
    });

    describe("getDialCode", function () {
      it("should return the default dial code if country code is empty", function () {
        var component = renderComponent();

        var result = component.getDialCode("");
        assert.equal(result, 46);
      });

      it("should return the default dial code if country code is unknown", function () {
        var component = renderComponent();

        var result = component.getDialCode("unknown");
        assert.equal(result, 46);
      });

      it("should return the dial code for a country code", function () {
        var component = renderComponent();

        var result = component.getDialCode("pl");
        assert.equal(result, 48);
      });
    });

    describe("guessSelectedCountry", function () {
      it("should return the empty country if phone number is empty", function () {
        var component = renderComponent();

        var result = component.guessSelectedCountry("");
        assert.equal(result.iso2, "unknown");
      });

      it("should return the empty country if phone number's dial code is unknown", function () {
        var component = renderComponent();

        var result = component.guessSelectedCountry("+2");
        assert.equal(result.iso2, "unknown");
      });

      it("should return a country from a phone number starting with +", function () {
        var component = renderComponent();

        var result = component.guessSelectedCountry("+48123456789");
        assert.equal(result.iso2, "pl");
      });

      it("should return a country from a phone number not starting with +", function () {
        var component = renderComponent();

        var result = component.guessSelectedCountry("48123456789");
        assert.equal(result.iso2, "pl");
      });
    });

    it("should collapse the dropdown", function () {
      var component = renderComponent();
      component.setState({expanded: true});

      component.collapse();
      assert.isFalse(component.state.expanded);
    });

    it("should call the onRemove callback when the closer is clicked", function () {
      var component = renderComponent();

      component.onCloserClick();
      assert.isTrue(component.props.onRemove.called);
    });

    describe("onCountryListItemSelect", function () {
      it("should replace the old dial code with new", function () {
        var component = renderComponent({value: "+46123456789"});

        component.onCountryListItemSelect("pl");
        assert.isTrue(component.props.onChange.calledWith("+48123456789"));
      });

      it("should prepend new dial code to the value if it doesn't contain the old dial code", function () {
        var component = renderComponent({value: "+2"});

        component.onCountryListItemSelect("pl");
        assert.isTrue(component.props.onChange.calledWith("+482"));
      });

      it("should prepend new dial code to the value if it doesn't start with a dial code", function () {
        var component = renderComponent({value: "2"});

        component.onCountryListItemSelect("pl");
        assert.isTrue(component.props.onChange.calledWith("+482"));
      });

      it("should collapse the dropdown", function () {
        var component = renderComponent();
        sinon.stub(component, "collapse");

        component.onCountryListItemSelect("pl");
        assert.isTrue(component.collapse.called);
      })
    });

    describe("onDocumentClick", function () {
      it("should not collapse if the target is the flag button", function () {
        var component = renderComponent();
        sinon.stub(component, "collapse");
        sinon.stub(component.refs.flagButton.getDOMNode(), "contains").returns(true);
        sinon.stub(component.refs.dropdown.getDOMNode(), "contains").returns(false);

        var target = document.createElement("div");
        component.onDocumentClick({target: target});
        assert.isFalse(component.collapse.called);
      });

      it("should not collapse if the target is in the dropdown", function () {
        var component = renderComponent();
        sinon.stub(component, "collapse");
        sinon.stub(component.refs.flagButton.getDOMNode(), "contains").returns(false);
        sinon.stub(component.refs.dropdown.getDOMNode(), "contains").returns(true);

        var target = document.createElement("div");
        component.onDocumentClick({target: target});
        assert.isFalse(component.collapse.called);
      });

      it("should not collapse if the target is outside", function () {
        var component = renderComponent();
        sinon.stub(component, "collapse");
        sinon.stub(component.refs.flagButton.getDOMNode(), "contains").returns(false);
        sinon.stub(component.refs.dropdown.getDOMNode(), "contains").returns(false);

        var target = document.createElement("div");
        component.onDocumentClick({target: target});
        assert.isTrue(component.collapse.called);
      });
    });

    describe("onFlagButtonClick", function () {
      var event = {};

      beforeEach(function () {
        event.preventDefault = sinon.stub();
        event.stopPropagation = sinon.stub();
      });

      it("should cancel the event", function () {
        var component = renderComponent();

        component.onFlagButtonClick(event);
        assert.isTrue(event.preventDefault.called);
        assert.isTrue(event.stopPropagation.called);
      });

      it("should not expand if it's read only", function () {
        var component = renderComponent({readonly: true});
        sinon.stub(component, "collapse");
        sinon.stub(component, "setState");

        component.onFlagButtonClick(event);
        assert.isFalse(component.collapse.called);
        assert.isFalse(component.setState.called);
      });

      it("should expand if it isn't", function () {
        var component = renderComponent();

        component.onFlagButtonClick(event);
        assert.isTrue(component.state.expanded);
      });

      it("should collapse if it's expanded", function () {
        var component = renderComponent();
        component.setState({expanded: true});
        sinon.stub(component, "collapse");

        component.onFlagButtonClick(event);
        assert.isTrue(component.collapse.called);
      });
    });

    it("should lose focus when the input loses focus", function () {
      var component = renderComponent();
      component.setState({focus: true});

      component.onInputBlur();
      assert.isFalse(component.state.focus);
    });

    describe("onInputChange", function () {
      it("should call the onChange callback when the value changes", function () {
        var component = renderComponent();

        component.onInputChange({target: {value: "+481"}});
        assert.isTrue(component.props.onChange.calledWith("+481"));
      });

      it("it should collapse multiple leading + chars", function () {
        var component = renderComponent();

        component.onInputChange({target: {value: "+++48123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+48123456789"));
      });

      it("should suppress the zero after dial code for Swedish phone numbers", function () {
        var component = renderComponent();
        component.setState({countryCode: "se"});

        component.onInputChange({target: {value: "+460123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+46123456789"));
      });

      it("should prepend the dial code if country is Sweden and phone number starts with zero", function () {
        var component = renderComponent();
        component.setState({countryCode: "se"});

        component.onInputChange({target: {value: "0123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+46123456789"));
      });

      it("should suppress the zero after dial code for Norwegian phone numbers", function () {
        var component = renderComponent();
        component.setState({countryCode: "no"});

        component.onInputChange({target: {value: "+470123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+47123456789"));
      });

      it("should prepend the dial code if country is Norway and phone number starts with zero", function () {
        var component = renderComponent();
        component.setState({countryCode: "no"});

        component.onInputChange({target: {value: "0123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+47123456789"));
      });

      it("should suppress the zero after dial code for Danish phone numbers", function () {
        var component = renderComponent();
        component.setState({countryCode: "dk"});

        component.onInputChange({target: {value: "+450123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+45123456789"));
      });

      it("should prepend the dial code if country is Denmark and phone number starts with zero", function () {
        var component = renderComponent();
        component.setState({countryCode: "dk"});

        component.onInputChange({target: {value: "0123456789"}});
        assert.isTrue(component.props.onChange.calledWith("+45123456789"));
      });
    });

    it("should become focused when the input becomes focused", function () {
      var component = renderComponent();
      component.setState({focus: false});

      component.onInputFocus();
      assert.isTrue(component.state.focus);
    });

    describe("onInputKeyDown", function () {
      var event = null;

      beforeEach(function () {
        event = {keyCode: undefined};
        event.preventDefault = sinon.stub();
        event.stopPropagation = sinon.stub();
      });

      it("should prevent the user from erasing the + char", function () {
        event.keyCode = 8;

        var component = renderComponent({value: "+"});

        component.onInputKeyDown(event);
        assert.isTrue(event.preventDefault.called);
        assert.isTrue(event.stopPropagation.called);
      });

      it("should not handle the TAB key if there's no onTab handler", function () {
        event.keyCode = 9;

        var component = renderComponent({onTab: undefined});
        component.setState({expanded: true, focus: true});

        component.onInputKeyDown(event);
        assert.isTrue(component.state.expanded);
        assert.isTrue(component.state.focus);
      });

      it("should handle the TAB key if there's onTab handler", function () {
        event.keyCode = 9;

        var component = renderComponent();
        component.setState({expanded: true, focus: true});

        component.onInputKeyDown(event);
        assert.isFalse(component.state.expanded);
        assert.isFalse(component.state.focus);
        assert.isTrue(component.props.onTab.calledWith(event));
      });
    });

    describe("render", function () {
      it("should render with arbitrary class if it's specified", function () {
        var component = renderComponent({className: "spam"});
        assert.isTrue(component.getDOMNode().classList.contains("spam"));
      });

      it("should not render as expanded if it isn't expanded", function () {
        var component = renderComponent();
        component.setState({expanded: false});

        assert.isFalse(component.getDOMNode().classList.contains("expanded"));
      });

      it("should render as expanded if it is expanded", function () {
        var component = renderComponent();
        component.setState({expanded: true});

        assert.isTrue(component.getDOMNode().classList.contains("expanded"));
      });

      it("should not render as read only if it isn't read only", function () {
        var component = renderComponent({readonly: false});
        assert.isFalse(component.getDOMNode().classList.contains("readonly"));
        assert.isFalse(component.refs.input.props.readOnly);
      });

      it("should render as read only if it is read only", function () {
        var component = renderComponent({readonly: true});
        assert.isTrue(component.getDOMNode().classList.contains("readonly"));
        assert.isTrue(component.refs.input.props.readOnly);
      });

      it("should configure and render the input", function () {
        var component = renderComponent({readonly: true});
        assert.isDefined(component.refs.input);
        assert.equal(
          component.refs.input.props.placeholder, component.props.infotext
        );
        assert.equal(
          component.refs.input.props.readOnly, component.props.readonly
        );
        assert.equal(component.refs.input.props.type, "text");
        assert.equal(
          component.refs.input.props.value, component.props.value
        );
        assert.equal(component.refs.input.props.onBlur, component.onInputBlur);
        assert.equal(
          component.refs.input.props.onChange, component.onInputChange
        );
        assert.equal(
          component.refs.input.props.onFocus, component.onInputFocus
        );
        assert.equal(
          component.refs.input.props.onKeyDown, component.onInputKeyDown
        );
      });

      it("should configure and render the flag button", function () {
        var component = renderComponent();
        assert.isDefined(component.refs.flagButton);
        assert.equal(
          component.refs.flagButton.props.onClick, component.onFlagButtonClick
        );
      });

      it("should render the flag icon for unknown country", function () {
        var component = renderComponent();

        var flagIcon = TestUtils.findRenderedDOMComponentWithClass(
          component.refs.flagButton, "flag-unknown"
        );
        assert.isDefined(flagIcon);
      });

      it("should render the flag icon for a country", function () {
        var component = renderComponent();
        component.setState({countryCode: "pl"});

        var flagIcon = TestUtils.findRenderedDOMComponentWithClass(
          component.refs.flagButton, "flag-pl"
        );
        assert.isDefined(flagIcon);
      });

      it("should configure and render the dropdown", function () {
        var component = renderComponent();
        assert.isDefined(component.refs.dropdown);
        assert.isArray(component.refs.dropdown.props.countries);
        assert.notEqual(
          component.refs.dropdown.props.countries, component.props.countries
        );
        assert.equal(
          component.refs.dropdown.props.expanded, component.state.expanded
        );
        assert.equal(
          component.refs.dropdown.props.onSelect,
          component.onCountryListItemSelect
        );
      });

      it("should configure and render the dropdown with custom countries", function () {
        var countries = [
          {
            dialCode: 48,
            iso2: "pl",
            name: "Poland"
          }
        ];

        var component = renderComponent({countries: countries});
        assert.equal(
          component.refs.dropdown.props.countries, component.props.countries
        );
      });

      it("should configure and render the closer", function () {
        var component = renderComponent();

        var closer = TestUtils.findRenderedDOMComponentWithClass(
          component, "closer"
        );
        assert.equal(closer.props.onClick, component.onCloserClick);
      });
    });
  });

  describe("CountryListItem", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          countryCode: "pl",
          highlighted: false,
          name: "Poland",
          onBecomeHighlighted: sinon.stub(),
          onResignHighlighted: sinon.stub(),
          onSelect: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(PhoneNumberInput.CountryListItem, actualProps),
        container
      );

      return component;
    };

    it("should call the onSelect callback when it's clicked", function () {
      var component = renderComponent();

      component.onClick();
      assert.isTrue(
        component.props.onSelect.calledWith(component.props.countryCode)
      );
    });

    it("should call the onBecomeHighlighted callback when mouse enters", function () {
      var component = renderComponent();

      component.onMouseEnter();
      assert.isTrue(component.props.onBecomeHighlighted.calledWith(
        component.props.countryCode
      ));
    });

    it("should call the onResignHighlighted callback when mouse leaves", function () {
      var component = renderComponent();

      component.onMouseLeave();
      assert.isTrue(component.props.onResignHighlighted.called);
    });

    describe("render", function () {
      it("should configure and render the container", function () {
        var component = renderComponent();

        var container = TestUtils.findRenderedDOMComponentWithTag(
          component, "li"
        );
        assert.equal(
          container.props["data-countrycode"], component.props.countryCode
        );
        assert.equal(container.props.onClick, component.onClick);
        assert.equal(container.props.onMouseEnter, component.onMouseEnter);
        assert.equal(container.props.onMouseLeave, component.onMouseLeave);
      });

      it("should render as highlighted if it's highlighted", function () {
        var component = renderComponent({highlighted: true});
        assert.isTrue(
          component.getDOMNode().classList.contains("highlighted")
        );
      });

      it("should render the flag icon", function () {
        var component = renderComponent();

        var flagIcon = TestUtils.findRenderedDOMComponentWithClass(
          component, "flag-pl"
        );
        assert.isDefined(flagIcon);
      });

      it("should render the country name", function () {
        var component = renderComponent();
        assert.equal(component.getDOMNode().innerText, component.props.name);
      });
    });
  });

  describe("CountryListDropdown", function () {
    var countries = [
      {
        name: "Denmark",
        iso2: "dk"
      },
      {
        name: "Norway",
        iso2: "no"
      },
      {
        name: "Poland",
        iso2: "pl"
      },
      {
        name: "Sweden",
        iso2: "se"
      }
    ];

    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          countries: countries,
          expanded: false,
          onSelect: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(PhoneNumberInput.CountryListDropdown, actualProps),
        container
      );

      return component;
    };

    it("should initialize state", function () {
      var component = renderComponent();
      assert.isNull(component.state.filteredCountries);
      assert.isNull(component.state.highlightedCountry);
      assert.equal(component.state.searchString, "");
    });

    it("should reset state when it becomes expanded", function () {
      var component = renderComponent({expanded: false});
      component.setState({
        filteredCountries: [countries[0]],
        highlightedCountry: "dk",
        searchString: "Den"
      });

      component.componentWillReceiveProps({expanded: true});
      assert.isNull(component.state.filteredCountries);
      assert.isNull(component.state.highlightedCountry);
      assert.equal(component.state.searchString, "");
    });

    describe("countries", function () {
      it("should return filtered countries", function () {
        var component = renderComponent();
        component.setState({
          filteredCountries: [countries[0]],
          highlightedCountry: "dk",
          searchString: "Den"
        });

        var result = component.countries();
        assert.equal(result, component.state.filteredCountries);
      });

      it("should return all countries", function () {
        var component = renderComponent();

        var result = component.countries();
        assert.equal(result, component.props.countries);
      });
    });

    it("should call the onSelect callback when an item is selected", function () {
      var component = renderComponent();

      component.onCountryListItemSelect("pl");
      assert.isTrue(component.props.onSelect.calledWith("pl"));
    });

    describe("onSearchInputChange", function () {
      var event = null;

      beforeEach(function () {
        event = {target: {value: ""}};
      });

      it("should clear filtered countries if search strings is empty", function () {
        var component = renderComponent();
        component.setState({filteredCountries: [countries[0]]});

        component.onSearchInputChange(event);
        assert.isNull(component.state.filteredCountries);
      });

      it("should set filtered countries according to the search strings", function () {
        event.target.value = "a";
        var component = renderComponent();

        component.onSearchInputChange(event);
        assert.isArray(component.state.filteredCountries);
        assert.lengthOf(component.state.filteredCountries, 3);
        assert.equal(component.state.filteredCountries[0].iso2, "dk");
        assert.equal(component.state.filteredCountries[1].iso2, "no");
        assert.equal(component.state.filteredCountries[2].iso2, "pl");
      });

      it("should clear the highlighted country", function () {
        var component = renderComponent();
        component.setState({highlightedCountry: "dk"});

        component.onSearchInputChange(event);
        assert.isNull(component.state.highlightedCountry);
      });

      it("should set the search string", function () {
        event.target.value = "a";
        var component = renderComponent();

        component.onSearchInputChange(event);
        assert.equal(component.state.searchString, "a");
      });
    });

    describe("onSearchInputKeyDown", function () {
      var event = null;

      beforeEach(function () {
        event = {keyCode: undefined};
        event.preventDefault = sinon.stub();
        event.stopPropagation = sinon.stub();
      });

      it("should cancel the event when arrow up is pressed", function () {
        event.keyCode = 38;

        var component = renderComponent();

        component.onSearchInputKeyDown(event);
        assert.isTrue(event.preventDefault.called);
        assert.isTrue(event.stopPropagation.called);
      });

      it("should cancel the event when arrow down is pressed", function () {
        event.keyCode = 40;

        var component = renderComponent();

        component.onSearchInputKeyDown(event);
        assert.isTrue(event.preventDefault.called);
        assert.isTrue(event.stopPropagation.called);
      });

      it("should highlight the first item if none is highlighted and arrow down is pressed", function () {
        event.keyCode = 40;

        var component = renderComponent();
        sinon.stub(component, "onCountryListItemBecomeHighlighted");

        component.onSearchInputKeyDown(event);
        assert.isTrue(component.onCountryListItemBecomeHighlighted.calledWith(
          "dk", true
        ));
      });

      it("should highlight the next item if first is highlighted and arrow down is pressed", function () {
        event.keyCode = 40;

        var component = renderComponent();
        component.setState({highlightedCountry: "dk"})
        sinon.stub(component, "onCountryListItemBecomeHighlighted");

        component.onSearchInputKeyDown(event);
        assert.isTrue(component.onCountryListItemBecomeHighlighted.calledWith(
          "no", true
        ));
      });

      it("should not highlight any item if last is highlighted and arrow down is pressed", function () {
        event.keyCode = 40;

        var component = renderComponent();
        component.setState({highlightedCountry: "se"})
        sinon.stub(component, "onCountryListItemBecomeHighlighted");

        component.onSearchInputKeyDown(event);
        assert.isFalse(component.onCountryListItemBecomeHighlighted.called);
      });

      it("should highlight the last item if none is highlighted and arrow up is pressed", function () {
        event.keyCode = 38;

        var component = renderComponent();
        sinon.stub(component, "onCountryListItemBecomeHighlighted");

        component.onSearchInputKeyDown(event);
        assert.isTrue(component.onCountryListItemBecomeHighlighted.calledWith(
          "se", true
        ));
      });

      it("should highlight the previous item if last is highlighted and up down is pressed", function () {
        event.keyCode = 38;

        var component = renderComponent();
        component.setState({highlightedCountry: "se"})
        sinon.stub(component, "onCountryListItemBecomeHighlighted");

        component.onSearchInputKeyDown(event);
        assert.isTrue(component.onCountryListItemBecomeHighlighted.calledWith(
          "pl", true
        ));
      });

      it("should not highlight any item if first is highlighted and up down is pressed", function () {
        event.keyCode = 38;

        var component = renderComponent();
        component.setState({highlightedCountry: "dk"})
        sinon.stub(component, "onCountryListItemBecomeHighlighted");

        component.onSearchInputKeyDown(event);
        assert.isFalse(component.onCountryListItemBecomeHighlighted.called);
      });

      it("should cancel the event if TAB key is pressed", function () {
        event.keyCode = 9;

        var component = renderComponent();

        component.onSearchInputKeyDown(event);
        assert.isTrue(event.preventDefault.called);
        assert.isTrue(event.stopPropagation.called);
      });

      it("should not call the onSelect callback if no item is highlighted and ENTER is pressed", function () {
        event.keyCode = 13;

        var component = renderComponent();

        component.onSearchInputKeyDown(event);
        assert.isFalse(component.props.onSelect.called);
      });

      it("should call the onSelect callback if an item is highlighted and ENTER is pressed", function () {
        event.keyCode = 13;

        var component = renderComponent();
        component.setState({highlightedCountry: "pl"})

        component.onSearchInputKeyDown(event);
        assert.isTrue(component.props.onSelect.calledWith("pl"));
      });
    });

    it("should update state when an item becomes highlighted", function () {
      var component = renderComponent();

      component.onCountryListItemBecomeHighlighted("pl");
      assert.equal(component.state.highlightedCountry, "pl");
    });

    it("should update state when an item resigns highlighted", function () {
      var component = renderComponent();

      component.onCountryListItemResignHighlighted();
      assert.isNull(component.state.highlightedCountry);
    });

    describe("render", function () {
      it("should not render as expanded if isn't expanded", function () {
        var component = renderComponent();
        assert.isFalse(component.getDOMNode().classList.contains("expanded"));
      });

      it("should render as expanded if it's expanded", function () {
        var component = renderComponent({expanded: true});
        assert.isTrue(component.getDOMNode().classList.contains("expanded"));
      });

      it("should configure and render the search input", function () {
        var component = renderComponent();

        var input = TestUtils.findRenderedDOMComponentWithTag(
          component, "input"
        );
        assert.equal(input.props.type, "text");
        assert.equal(input.props.onChange, component.onSearchInputChange);
        assert.equal(input.props.onKeyDown, component.onSearchInputKeyDown);
      });

      it("should configure and render the items", function () {
        var component = renderComponent();

        var items = TestUtils.scryRenderedComponentsWithType(
          component, PhoneNumberInput.CountryListItem
        );
        assert.lengthOf(items, 4);

        var item = items[0];
        assert.equal(item.props.countryCode, countries[0].iso2);
        assert.isFalse(item.props.highlighted);
        assert.equal(item.props.name, countries[0].name);
        assert.equal(
          item.props.onBecomeHighlighted,
          component.onCountryListItemBecomeHighlighted
        );
        assert.equal(
          item.props.onResignHighlighted,
          component.onCountryListItemResignHighlighted
        );
      });

      it("should configure and render the highlighted item", function () {
        var component = renderComponent();
        component.setState({highlightedCountry: "pl"})

        var items = TestUtils.scryRenderedComponentsWithType(
          component, PhoneNumberInput.CountryListItem
        );

        var item = items[2];
        assert.isTrue(item.props.highlighted);
      });
    });
  });
});
