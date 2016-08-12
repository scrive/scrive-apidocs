
module.exports = {
  isProgressStatus: function (status) {
    return status == "complete"
      || status == "user_sign"
      || status == "outstanding_transaction"
      || status == "no_client"
      || status == "started";
  },

  isStatusOutstanding: function (status) {
    return status == "outstanding_transaction";
  },

  isStatusNoClient: function (status) {
    return status == "no_client";
  },

  isStatusComplete: function (status) {
    return status == "complete";
  },

  isFaultStatus: function (status) {
    return status == "invalid_parameters"
      || status == "already_in_progress"
      || status == "access_denied_rp"
      || status == "retry"
      || status == "internal_error"
      || status == "expired_transaction"
      || status == "user_cancel"
      || status == "client_err"
      || status == "certificate_err"
      || status == "cancelled"
      || status == "start_failed";
  },

  statusMessage: function (thisDevice, status, signatory) {
    var self = this;
    var canChangeSSN = !signatory.personalnumberField().isClosed();
    if (status == undefined && thisDevice) {
      return localization.docsignview.eleg.bankid.rfa13;
    } else if (status == undefined  && !thisDevice) {
      return localization.docsignview.eleg.bankid.rfa1;
    } else if (status == "no_client") {
      return localization.docsignview.eleg.bankid.rfa1;
    } else if (status == "already_in_progress") {
      return localization.docsignview.eleg.bankid.rfa3;
    } else if (status == "cancelled") {
      return localization.docsignview.eleg.bankid.rfa3;
    } else if (status == "retry") {
      return localization.docsignview.eleg.bankid.rfa5;
    } else if (status == "internal_error") {
      return localization.docsignview.eleg.bankid.rfa5;
    } else if (status == "user_cancel") {
      return localization.docsignview.eleg.bankid.rfa6;
    } else if (status == "expired_transaction") {
      return localization.docsignview.eleg.bankid.rfa8;
    } else if (status == "user_sign") {
      return localization.docsignview.eleg.bankid.rfa9;
    } else if (status == "client_err") {
      return localization.docsignview.eleg.bankid.rfa12;
      // If RP tried to start the client automatically, the RP should inform the
      // user that the app is starting. Message RFA13 should be used.
      // If RP did not try to start the client automatically [...] RFA1 should be used.
    } else if (status == "outstanding_transaction" && thisDevice) {
      return localization.docsignview.eleg.bankid.rfa13;
    } else if (status == "outstanding_transaction" && !thisDevice) {
      return localization.docsignview.eleg.bankid.rfa1;
    } else if (status == "started") {
      return localization.docsignview.eleg.bankid.rfa14;
    } else if (status == "certificate_err") {
      return localization.docsignview.eleg.bankid.rfa16;
    } else if (status == "start_failed") {
      return localization.docsignview.eleg.bankid.rfa17;
      // RP must not try the same request again. This is an internal error
      // within RP"s system and must not be communicated to the user as a BankID-error.
    } else if (status == "invalid_parameters" && canChangeSSN) {
      return localization.docsignview.eleg.bankid.invalidParametersCanChange;
    } else if (status == "invalid_parameters" && !canChangeSSN) {
      return localization.docsignview.eleg.bankid.invalidParametersCantChange;
    } else if (status == "access_denied_rp") {
      return localization.docsignview.eleg.bankid.accessDenied;
    } else {
      return "";
    }
  },

  normalizedPersonalNumber: function (signatory) {
    var pp = signatory.personalnumber();
    pp = pp.replace(/-/g, "");
    if (pp.length <= 10) {
      return "19" + pp;
    } else {
      return pp;
    }
  }
};
