import React from "react";
import classNames from "classnames";
import $ from "jquery";
import BackboneMixin from "../../common/backbone_mixin";
import HtmlTextWithSubstitution from "../../common/htmltextwithsubstitution";
import InfoTextInput from "../../common/infotextinput";
import DataRetention from "./dataretention";

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  propTypes: {
    model: React.PropTypes.instanceOf(DataRetention).isRequired,
    status: React.PropTypes.string.isRequired,
    text: React.PropTypes.string.isRequired,
    description: React.PropTypes.string.isRequired
  },

  getBackboneModels: function () {
    return [this.props.model];
  },

  maxDays: function () {
    return this.props.model.maxDaysFor(this.props.status);
  },

  value: function () {
    return this.props.model.idleDocTimeout(this.props.status);
  },

  hasValue: function () {
    return !!this.value();
  },

  isValid: function () {
    return this.props.model.isIdleDocTimeoutValid(this.props.status);
  },

  onValueChange: function (value) {
    this.props.model.setIdleDocTimeout(this.props.status, value);
  },

  onCheckBoxClick: function () {
    if (this.hasValue()) {
      this.onValueChange(null);
    } else {
      this.onValueChange(this.maxDays());
    }
  },

  onInputChange: function (newValue) {
    this.onValueChange(newValue);
  },

  render: function () {
    const checkboxClassName = classNames("checkbox", {
      checked: this.hasValue()
    });

    const inputClassName = classNames({
      error: !this.isValid()
    });

    const $text = $("<div>" + this.props.text + "</div>");
    // Split the text around <span class='put-field'/>.
    var mainTextBefore = "";
    var mainTextAfter = "";
    var putFieldEncountered = false;
    // See https://developer.mozilla.org/en-US/docs/Web/API/NodeList
    Array.prototype.forEach.call($text.get(0).childNodes, function (el) {
      if (el.className == 'put-field') {
        putFieldEncountered = true;
      } else {
        if (putFieldEncountered) {
          mainTextAfter = mainTextAfter + " " + el.textContent;
        } else {
          mainTextBefore = mainTextBefore + " " + el.textContent;
        }
      }
    });

    const maxText = this.maxDays() == 365
      ? localization.account.dataRetention.maximum
      : localization.account.dataRetention.companyMaximum;

    return (
      <div className="data-retention-option-wrapper">
        <div className="data-retention-option-checkbox">
          <div
            className={checkboxClassName}
            onClick={this.onCheckBoxClick}
            >
              <div className="checkmark" />
          </div>
        </div>
        <div className="data-retention-option-main-text">
          {mainTextBefore}
          <InfoTextInput
            type="numeric"
            value={this.value()}
            onChange={this.onInputChange}
            className={inputClassName}
          />
           {mainTextAfter}
           {" "}
           <HtmlTextWithSubstitution
             secureText={maxText}
             subs={{".put-max-days": this.maxDays()}}
           />
        </div>
        <div className="data-retention-option-description">
          {this.props.description}
        </div>
      </div>
    );
  }
});
