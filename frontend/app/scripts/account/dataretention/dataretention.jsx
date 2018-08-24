import Backbone from "backbone";
import _ from "underscore";
import {Submit} from "../../../js/submits";

module.exports = Backbone.Model.extend({
  initialize: function () {
    this.url = "/api/frontend/dataretentionpolicy";
    this.changeurl = "/api/frontend/dataretentionpolicy/set";
    this.fetch();
  },

  policy: function () {
    const policy = this.get("data_retention_policy");
    // Overwrite immediate_trash when getting the user's policy. It's possible
    // that it is wrong if the user's policy had been saved before the company's
    // immediate trash setting was set to true.
    if (policy) {
      policy.immediate_trash =
        policy.immediate_trash || this.companyImmediateTrash();
    }
    return policy;
  },

  companyPolicy: function () {
    return this.get("company_data_retention_policy");
  },

  getInPolicy: function (name) {
    const policy = this.policy();
    const value = policy ? policy[name] : undefined;
    return value === undefined ? null : value;
  },

  getInCompanyPolicy: function (name) {
    const policy = this.companyPolicy();
    const value = policy ? policy[name] : undefined;
    return value === undefined ? null : value;
  },

  setInPolicy: function (name, value) {
    var policy = this.policy();
    if (policy) {
      policy[name] = value;
      this.trigger("change");
    }
  },

  toggleImmediateTrash: function () {
    if (!this.companyImmediateTrash()) {
      const current = this.getInPolicy("immediate_trash");
      this.setInPolicy("immediate_trash", !current);
    }
  },

  immediateTrash: function () {
    return this.getInPolicy("immediate_trash");
  },

  companyImmediateTrash: function () {
    return this.getInCompanyPolicy("immediate_trash");
  },

  idleDocTimeout: function (status) {
    return this.getInPolicy("idle_doc_timeout_" + status);
  },

  companyIdleDocTimeout: function (status) {
    return this.getInCompanyPolicy("idle_doc_timeout_" + status);
  },

  setIdleDocTimeout: function (status, value) {
    const res = parseInt(value);
    // If it's not parsable, we still store the string.
    const newValue = isNaN(res) ? (value == "" ? null : value) : res;
    this.setInPolicy("idle_doc_timeout_" + status, newValue);
  },

  maxDaysFor: function (status) {
    return this.companyIdleDocTimeout(status) || 365;
  },

  isIdleDocTimeoutValid: function (status) {
    const value = this.idleDocTimeout(status);
    const isValidNumber = !isNaN(parseInt(value))
      && 1 <= value && value <= this.maxDaysFor(status);
    return !value || isValidNumber;
  },

  isValid: function () {
    const self = this;
    const statuses =
      ["preparation", "closed", "canceled", "timedout", "rejected", "error"];

    return _.all(statuses, function (status) {
      return self.isIdleDocTimeoutValid(status);
    });
  },

  save: function(success, error) {
    if (this.isValid()) {
      var self = this;
      new Submit({
        url : self.changeurl,
        method : "POST",
        ajax : true,
        ajaxsuccess : function() {
          self.set({dirty: false});
          if(success) {
            success();
          }
        },
        ajaxerror: error,
        data_retention_policy: JSON.stringify(self.policy())
      }).send();
    }
  }
});
