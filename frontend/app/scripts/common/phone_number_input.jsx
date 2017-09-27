var classNames = require("classnames");
var countryTelephoneData = require("country-telephone-data");
var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;

var DEFAULT_COUNTRIES = countryTelephoneData.allCountries;
var DEFAULT_COUNTRY_CODE = "se";
var DEFAULT_COUNTRY = _.find(
  countryTelephoneData.allCountries,
  function (item) {
    return item.iso2 == DEFAULT_COUNTRY_CODE;
  }
);
var EMPTY_COUNTRY = {
  dialCode: "",
  iso2: "unknown",
  name: ""
};
var KEY_CODE_ARROW_DOWN = 40;
var KEY_CODE_ARROW_UP = 38;
var KEY_CODE_BACKSPACE = 8;
var KEY_CODE_ENTER = 13;
var KEY_CODE_TAB = 9;

var CountryListItem = React.createClass({
  propTypes: {
    countryCode: React.PropTypes.string.isRequired,
    highlighted: React.PropTypes.bool.isRequired,
    name: React.PropTypes.string.isRequired,
    onBecomeHighlighted: React.PropTypes.func.isRequired,
    onResignHighlighted: React.PropTypes.func.isRequired,
    onSelect: React.PropTypes.func.isRequired
  },
  onClick: function (event) {
    this.props.onSelect(this.props.countryCode);
  },
  onMouseEnter: function (event) {
    this.props.onBecomeHighlighted(this.props.countryCode);
  },
  onMouseLeave: function (event) {
    this.props.onResignHighlighted();
  },
  render: function () {
    var className = classNames({highlighted: this.props.highlighted});
    var flagClassName = classNames("flag", "flag-" + this.props.countryCode);

    return (
      <li
        className={className}
        data-countrycode={this.props.countryCode}
        onClick={this.onClick}
        onMouseEnter={this.onMouseEnter}
        onMouseLeave={this.onMouseLeave}
      >
        <span className={flagClassName} />{this.props.name}
      </li>
    );
  }
});

var CountryListDropdown = React.createClass({
  propTypes: {
    countries: React.PropTypes.array.isRequired,
    expanded: React.PropTypes.bool.isRequired,
    onSelect: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      filteredCountries: null,
      highlightedCountry: null,
      searchString: ""
    };
  },
  componentWillReceiveProps: function (nextProps) {
    if (!this.props.expanded && nextProps.expanded) {
      this.setState(this.getInitialState());
    } else if (this.props.expanded && !nextProps.expanded) {
      this.refs.scrollView.getDOMNode().scrollTop = 0;
    }
  },
  countries: function () {
    if (_.isArray(this.state.filteredCountries)) {
     return this.state.filteredCountries;
    }

    return this.props.countries;
  },
  onCountryListItemSelect: function (newCountryCode) {
    this.props.onSelect(newCountryCode);
  },
  onSearchInputChange: function (event) {
    this.refs.scrollView.getDOMNode().scrollTop = 0;

    var searchString = event.target.value;
    var filteredCountries = null;
    if (searchString) {
      var searchRegex = new RegExp(searchString, "i");
      filteredCountries = _.filter(
        this.props.countries,
        function (countrySpec) {
          return searchRegex.test(countrySpec.name);
        }
      );
    }

    this.setState({
      filteredCountries: filteredCountries,
      highlightedCountry: null,
      searchString: searchString
    });
  },
  onSearchInputKeyDown: function (event) {
    if (event.keyCode == KEY_CODE_ARROW_DOWN || event.keyCode == KEY_CODE_ARROW_UP) {
      event.stopPropagation();
      event.preventDefault();

      var self = this;
      var countries = this.countries();
      var newCountryCode = null;

      var currentCountryIndex = _.findIndex(countries, {
        iso2: self.state.highlightedCountry
      });

      if (currentCountryIndex == -1) {
        if (event.keyCode == KEY_CODE_ARROW_DOWN) {
          newCountryCode = countries[0].iso2;
        } else {
          newCountryCode = countries[countries.length - 1].iso2
        }
      } else {
        if (event.keyCode == KEY_CODE_ARROW_DOWN) {
          if (currentCountryIndex < countries.length - 1) {
            newCountryCode = countries[currentCountryIndex + 1].iso2;
          }
        } else {
          if (currentCountryIndex > 0) {
            newCountryCode = countries[currentCountryIndex - 1].iso2;
          }
        }
      }

      if (newCountryCode) {
        this.onCountryListItemBecomeHighlighted(newCountryCode, true);
      }
    } else if (event.keyCode == KEY_CODE_TAB) {
      event.stopPropagation();
      event.preventDefault();
    } else if (event.keyCode == KEY_CODE_ENTER) {
      if (this.state.highlightedCountry) {
        this.props.onSelect(this.state.highlightedCountry);
      }
    }
  },
  onCountryListItemBecomeHighlighted: function (countryCode, scroll) {
    if (countryCode && scroll) {
      var $scrollView = $(this.refs.scrollView.getDOMNode());
      var $newHighligtedItem = $(
        'li[data-countrycode="' + countryCode + '"]', $scrollView
      );

      if ($newHighligtedItem.length > 0) {
        $scrollView.scrollTop($newHighligtedItem.position().top);
      }
    }

    this.setState({highlightedCountry: countryCode});
  },
  onCountryListItemResignHighlighted: function () {
    this.setState({highlightedCountry: null});
  },
  render: function () {
    var self = this;

    var className = classNames("dropdown", {
      expanded: this.props.expanded
    });

    return (
      <div ref="dropdown" className={className}>
        <input
          type="text"
          value={this.state.searchString}
          onChange={this.onSearchInputChange}
          onKeyDown={this.onSearchInputKeyDown}
        />
        <div ref="scrollView" className="scrollView">
          <ul ref="dropdownMenu">
            {_.map(this.countries(), function (countrySpec, index) {
              return (
                <CountryListItem
                  key={index}
                  countryCode={countrySpec.iso2}
                  highlighted={self.state.highlightedCountry == countrySpec.iso2}
                  name={countrySpec.name}
                  onBecomeHighlighted={self.onCountryListItemBecomeHighlighted}
                  onResignHighlighted={self.onCountryListItemResignHighlighted}
                  onSelect={self.onCountryListItemSelect}
                />
              );
            })}
          </ul>
        </div>
      </div>
    );
  }
});

var PhoneNumberInput = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    className: React.PropTypes.string,
    countries: React.PropTypes.array,
    infotext: React.PropTypes.string,
    readonly: React.PropTypes.bool,
    value: React.PropTypes.string,
    onChange: React.PropTypes.func.isRequired,
    onRemove: React.PropTypes.func,
    onTab: React.PropTypes.func
  },
  getInitialState: function () {
    return {
      countryCode: this.guessSelectedCountry(this.props.value).iso2,
      expanded: false,
      focus: false
    };
  },
  componentDidMount: function () {
    document.addEventListener("click", this.onDocumentClick);

    if (!this.props.value) {
      this.props.onChange("+" + this.getDialCode(this.state.countryCode));
    }
  },
  componentWillUnmount: function () {
    document.removeEventListener("click", this.onDocumentClick);
  },
  componentWillReceiveProps: function (nextProps) {
    if (this.props.value != nextProps.value) {
      var newCountry = this.guessSelectedCountry(nextProps.value);
      this.setState({countryCode: newCountry.iso2});
    }
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevProps.value != this.props.value) {
      if (!this.props.value) {
        this.props.onChange("+" + this.getDialCode(DEFAULT_COUNTRY.iso2));
      } else if (this.props.value.substr(0, 1) != "+") {
        this.props.onChange(
          "+" + this.getDialCode(DEFAULT_COUNTRY.iso2) + this.props.value
        );
      }
    }
  },
  getDialCode: function (countryCode) {
    var countryIdx = countryTelephoneData.iso2Lookup[countryCode];
    if (_.isUndefined(countryIdx)) {
      countryIdx = countryTelephoneData.iso2Lookup[DEFAULT_COUNTRY.iso2];
    }

    return countryTelephoneData.allCountries[countryIdx].dialCode;
  },
  guessSelectedCountry: function(inputNumber) {
    var secondBestGuess = EMPTY_COUNTRY;

    if (inputNumber.substr(0, 1) == "+") {
      inputNumber = inputNumber.substr(1, 6);
    } else {
      inputNumber = inputNumber.substr(0, 6);
    }

    var allCountryCodes = countryTelephoneData.allCountryCodes;

    var inputNumberForCountries = inputNumber.substr(0, 4);
    if (inputNumber.trim() !== "") {
      var bestGuess = _.reduce(
        DEFAULT_COUNTRIES,
        function (selectedCountry, country) {
          if (allCountryCodes[inputNumberForCountries] && allCountryCodes[inputNumberForCountries][0] === country.iso2) {
            // if the country dialCode exists WITH area code
            return country;
          } else if (allCountryCodes[inputNumberForCountries] && allCountryCodes[inputNumberForCountries][0] === selectedCountry.iso2) {
            // if the selected country dialCode is there with the area code
            return selectedCountry;
          } else {
            if (inputNumber.substr(0, country.dialCode.length) == country.dialCode) {
              if (country.dialCode.length > selectedCountry.dialCode.length) {
                return country;
              }

              if (country.dialCode.length === selectedCountry.dialCode.length && country.priority < selectedCountry.priority) {
                return country;
              }
            }
          }
          return selectedCountry;
        },
        {dialCode: "", priority: 10001}
      );
    } else {
      return secondBestGuess;
    }

    if (!bestGuess.name) {
      return secondBestGuess;
    }

    return bestGuess;
  },
  collapse: function () {
    this.setState({expanded: false});
  },
  onCloserClick: function (event) {
    if (_.isFunction(this.props.onRemove)) {
      this.props.onRemove();
    }
  },
  onCountryListItemSelect: function (newCountryCode) {
    var newDialCode = this.getDialCode(newCountryCode);
    var newPhoneNumber = "+" + newDialCode;

    if (this.props.value) {
      var oldDialCode = this.getDialCode(this.state.countryCode);

      if (this.props.value.substr(0, oldDialCode.length + 1) == "+" + oldDialCode) {
        newPhoneNumber = this.props.value.replace(
          "+" + oldDialCode, "+" + newDialCode
        );
      } else if (this.props.value.substr(0, 1) == "+") {
        newPhoneNumber = (
          "+" + newDialCode + this.props.value.substr(
            1, this.props.value.length - 1
          )
        );
      } else {
        newPhoneNumber = "+" + newDialCode + this.props.value;
      }
    }

    this.collapse();
    this.props.onChange(newPhoneNumber);
  },
  onDocumentClick: function (event) {
    var shouldCollapse = !(
      this.refs.flagButton.getDOMNode().contains(event.target) ||
      this.refs.dropdown.getDOMNode().contains(event.target)
    );

    if (shouldCollapse) {
      this.collapse();
    }
  },
  onFlagButtonClick: function (event) {
    event.stopPropagation();
    event.preventDefault();

    if (!this.props.readonly) {
      if (!this.state.expanded) {
        this.setState({expanded: true});
      } else {
        this.collapse();
      }
    }
  },
  onInputBlur: function () {
    this.setState({focus: false});
  },
  onInputChange: function (event) {
    var sanitizedValue = event.target.value.replace(/^\++/, "+");

    if (this.state.countryCode == "se" || this.state.countryCode == "no" || this.state.countryCode == "dk") {
      var dialCode = this.getDialCode(this.state.countryCode);

      if (sanitizedValue == "0") {
        sanitizedValue = "+" + dialCode;
      } else if (sanitizedValue.substr(0, 1) == "0") {
        sanitizedValue = "+" + dialCode + sanitizedValue.substr(1, sanitizedValue.length - 1);
      } else if (sanitizedValue.substr(0, 4) == "+" + dialCode + "0") {
        sanitizedValue = "+" + dialCode + sanitizedValue.substr(4, sanitizedValue.length - 1);
      }
    }

    this.props.onChange(sanitizedValue);
  },
  onInputFocus: function () {
    this.setState({focus: true});
  },
  onInputKeyDown: function (event) {
    if (event.keyCode == KEY_CODE_BACKSPACE && this.props.value == "+") {
      event.preventDefault();
      event.stopPropagation();
    } else if (event.keyCode == KEY_CODE_TAB && _.isFunction(this.props.onTab)) {
      this.setState({expanded: false, focus: false});
      this.props.onTab(event);
    }
  },
  render: function () {
    var self = this;

    var fakePlaceholder = (
      BrowserInfo.isIE9orLower() && !this.state.focus && !this.props.value
    );

    var containerClassName = classNames(
      "info-text-input", "phone-number-input", this.props.className || "", {
        expanded: this.state.expanded,
        readonly: this.props.readonly
      }
    );

    var flagClassName = classNames("flag", "flag-" + this.state.countryCode);

    return (
      <div className={containerClassName}>
        <input
          ref="input"
          placeholder={this.props.infotext}
          readOnly={this.props.readonly}
          type="text"
          value={fakePlaceholder ? this.props.infotext : this.props.value}
          onBlur={this.onInputBlur}
          onChange={this.onInputChange}
          onFocus={this.onInputFocus}
          onKeyDown={this.onInputKeyDown}
        />
        <div ref="flagButton" className="flag-button" onClick={this.onFlagButtonClick}>
          <span className={flagClassName} />
        </div>
        <CountryListDropdown
          ref="dropdown"
          countries={this.props.countries || DEFAULT_COUNTRIES}
          expanded={this.state.expanded}
          onSelect={this.onCountryListItemSelect}
        />
        <div className="closer" onClick={this.onCloserClick} />
      </div>
    );
  }
});

module.exports = {
  CountryListDropdown: CountryListDropdown,
  CountryListItem: CountryListItem,
  PhoneNumberInput: PhoneNumberInput
};
