/** @jsx React.DOM */
/* Diffrent utils used by archive view to parse documents */

define(['legacy_code'], function() {

  var e = {};
  // Utils for working with list call

  e.listCallUrl = "/api/v2/documents/list";
  e.dataFetcher=function(d) {return d.documents;};
  e.idFetcher=function(d) {return d.field("id");};
  e.totalCountFunction=function(data){ return data.total_matching;};
  e.maxPageSize=100;
  e.paramsFunctionWithFilter = function(f) {
    return function(text,selectfiltering,sorting, offset, maxPageSize) {
      var filters = _.clone(f || []);
      if (text) {
        filters.push({"filter_by" : "text", "text" : text});
      }
      if (selectfiltering) {
        _.each(selectfiltering.filters(), function(f) {
          filters = filters.concat(f.value);
        });
      }
      var sortingBy;
      if (sorting.current()) {
        sortingBy = {
          sort_by : sorting.current(),
          order : sorting.isAsc() ? "ascending" : "descending"
        };
      } else {
        sortingBy = {
          sort_by : "mtime",
          order: "descending"
        };
      }
      return {
        filter : JSON.stringify(filters),
        sorting: JSON.stringify([sortingBy]),
        offset : offset,
        max : maxPageSize
      };
    }
  };

  // Utils for fetching data from document
  e.signatoryTime = function(s) {
    return s.sign_time || s.rejected_time || s.seen_time || s.read_invitation_time;
  };

  e.signatoryName = function(s) {
    var nameFields = _.filter(s.fields, function(f) {
        return f.type ==="name" && f.value;
    });
    nameFields = _.sortBy(nameFields, function(f) {
      return f.order;
    });
    return _.map(nameFields,function(f) {return f.value;}).join(" ");
  };

  e.signatoryEmail = function(s) {
    var emailField = _.find(s.fields, function(f) {
      return f.type ==="email";
    });
    if (emailField) {
      return emailField.value;
    } else {
      return "";
    }
  };

  e.signatoryPhone = function(s) {
    var phoneField = _.find(s.fields, function(f) {
      return f.type ==="mobile";
    });
    if (phoneField) {
      return phoneField.value;
    } else {
      return "";
    }
  };

  e.signatoryCompany = function(s) {
    var companyField = _.find(s.fields, function(f) {
      return f.type ==="company";
    });
    if (companyField) {
      return companyField.value;
    } else {
      return "";
    }
  };

  e.signatorySmartName = function(s) {
    if (s) {
      return e.signatoryName(s) || e.signatoryEmail(s) || e.signatoryPhone(s) || localization.notNamedParty;
    }
    else {
      return "undefined";
    }
  };

  e.currentViewerParty = function(d) {
    return _.find(d.field("parties"), function(s) {
      return s.id == d.field("viewer").signatory_id;
    });
  };

  e.documentAuthor = function(d) {
    return _.find(d.field("parties"), function(s) {
      return s.is_author;
    });
  };

  e.viewerIsAuthor = function(d) {
    return e.documentAuthor(d).id == d.field("viewer").signatory_id;
  };

  e.signatoryStatus = function(d,s) {
    if (d.field("status") === "document_error") {
      return "problem";
    } else if (d.field("status") === "preparation") {
      return "draft";
    }  else if (s.sign_time) {
      return "signed";
    } else if (d.field("status") === "canceled") {
      return "cancelled";
    }  else if (d.field("status") === "timedout") {
      return "timeouted";
    }  else if (d.field("status") === "rejected") {
      return "rejected";
    } else if (s.seen_time) {
      return "opened";
    } else if (s.read_invitation_time) {
      return "read";
    } else if (s.email_delivery_status === "not_delivered" || s.mobile_delivery_status === "not_delivered") {
      return "deliveryproblem";
    } else if (s.email_delivery_status === "delivered" || s.mobile_delivery_status === "delivered") {
      return "delivered";
    } else {
      return "sent";
    }
  };

  e.signatoryCanSignNow = function(d,s) {
    var so = s.sign_order;
    _.each(d.field("parties"),function(sig) {
       if (sig.is_signatory && !sig.sign_time) {
        so = Math.min(so,sig.sign_order);
       }
    });
    return d.field("status") === "pending" && !s.sign_time && s.is_signatory && so == s.sign_order;
  };

  e.documentStatus = function(d) {
    var signingParties = _.filter(d.field("parties"), function(s) {
      return s.is_signatory;
    });
    var someSignatoryHasDeliveryProblem = _.some(signingParties, function(s) {
      return s.email_delivery_status === "not_delivered" || s.mobile_delivery_status === "not_delivered";
    });
    var everySignatoryOpened = _.every(signingParties, function(s) {
      return s.seen_time || s.sign_time;
    });
    var everySignatoryReadInvitation = _.every(signingParties, function(s) {
      return s.read_invitation_time || s.seen_time || s.sign_time;
    });
    var everySignatoryDelivered = _.every(signingParties, function(s) {
      // We handle the author separately as we often end up in the situation 
      // where the author is invited with email, sign_order=1 and then we 
      // never get a delivery report, so we end up with email_delivery_status = unknown.
      // This method doesn't care about sign orders, so this is OK.
      return s.delivery_method === "api" || 
             s.delivery_method === "pad" ||
             s.is_author ||
             s.email_delivery_status === "delivered" || 
             s.mobile_delivery_status === "delivered";
    });

    if (d.field("status") === "document_error") {
      return "problem";
    } else if (d.field("status") === "preparation") {
      return "draft";
    }  else if (d.field("status") === "closed") {
      return "signed";
    } else if (d.field("status") === "canceled") {
      return "cancelled";
    }  else if (d.field("status") === "timedout") {
      return "timeouted";
    }  else if (d.field("status") === "rejected") {
      return "rejected";
    } else if (someSignatoryHasDeliveryProblem) {
      return "deliveryproblem";
    } else if (everySignatoryOpened) {
      return "opened";
    } else if (everySignatoryReadInvitation) {
      return "read";
    } else if (everySignatoryDelivered) {
      return "delivered";
    } else {
      return "sent";
    }
  };


  e.documentLink = function(d) {
    return "/d/" + d.field("id");
  };

  e.documentParty = function(d) {
    var signingParties = _.filter(d.field("parties"), function(s) {
      return s.is_signatory;
    });
    var signingPartiesSmartNames =  _.map(signingParties, function(s) {
      return e.signatorySmartName(s);
    });
    return signingPartiesSmartNames.join(", ");
  };

  e.documentDeliveryText = function(d) {
    var dms = _.map(d.field("parties"),function(s) {
      var dm = s.delivery_method;
      if (dm == "email") {
        return capitaliseFirstLetter(localization.delivery.email);
      } else if (dm == "pad") {
        return capitaliseFirstLetter(localization.delivery.pad);
      } else if (dm == "mobile") {
        return capitaliseFirstLetter(localization.delivery.mobile);
      } else if (dm == "email_mobile") {
        return capitaliseFirstLetter(localization.delivery.email_mobile);
      } else if (dm == "api") {
        return capitaliseFirstLetter(localization.delivery.api);
      } else {
        return "";
      }
    });
    dms = _.uniq(dms);
    dms.sort();

    var text = dms[0] || "";
    for(var i =1 ; i< dms.length; i++)
      text += ", " + dms[i];
    return text;
  };

  return e;

});
